/* NOTE: most of this code is a direct copy of the qemu tap code
*/

#include <stdio.h>
#include <wchar.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winioctl.h>
#include <ws2tcpip.h>
#include <iphlpapi.h>
#include <io.h>
#include <Fcntl.h>

//=============
// TAP IOCTLs
//=============

#define TAP_CONTROL_CODE(request,method) \
  CTL_CODE (FILE_DEVICE_UNKNOWN, request, method, FILE_ANY_ACCESS)

#define TAP_IOCTL_GET_MAC               TAP_CONTROL_CODE (1, METHOD_BUFFERED)
#define TAP_IOCTL_GET_VERSION           TAP_CONTROL_CODE (2, METHOD_BUFFERED)
#define TAP_IOCTL_GET_MTU               TAP_CONTROL_CODE (3, METHOD_BUFFERED)
#define TAP_IOCTL_GET_INFO              TAP_CONTROL_CODE (4, METHOD_BUFFERED)
#define TAP_IOCTL_CONFIG_POINT_TO_POINT TAP_CONTROL_CODE (5, METHOD_BUFFERED)
#define TAP_IOCTL_SET_MEDIA_STATUS      TAP_CONTROL_CODE (6, METHOD_BUFFERED)
#define TAP_IOCTL_CONFIG_DHCP_MASQ      TAP_CONTROL_CODE (7, METHOD_BUFFERED)
#define TAP_IOCTL_GET_LOG_LINE          TAP_CONTROL_CODE (8, METHOD_BUFFERED)
#define TAP_IOCTL_CONFIG_DHCP_SET_OPT   TAP_CONTROL_CODE (9, METHOD_BUFFERED)

//=================
// Registry keys
//=================

#define ADAPTER_KEY "SYSTEM\\CurrentControlSet\\Control\\Class\\{4D36E972-E325-11CE-BFC1-08002BE10318}"

#define NETWORK_CONNECTIONS_KEY "SYSTEM\\CurrentControlSet\\Control\\Network\\{4D36E972-E325-11CE-BFC1-08002BE10318}"

//======================
// Filesystem prefixes
//======================

#define USERMODEDEVICEDIR "\\\\.\\Global\\"
#define TAPSUFFIX         ".tap"


//======================
// Compile time configuration
//======================

//#define DEBUG_TAP_WIN32 1

#define TUN_ASYNCHRONOUS_WRITES 1

#define TUN_BUFFER_SIZE 1560
#define TUN_MAX_BUFFER_COUNT 32
 

#include "help.h"

int open_tap(ip4_addr_t local_ip, ip4_addr_t local_mask, struct tap_info * ti);
void close_tap(int tap_fd);


static int is_tap_win32_dev(const char *guid)
{
    HKEY netcard_key;
    LONG status;
    DWORD len;
    int i = 0;

    status = RegOpenKeyEx(
        HKEY_LOCAL_MACHINE,
        ADAPTER_KEY,
        0,
        KEY_READ,
        &netcard_key);

    if (status != ERROR_SUCCESS) {
        return FALSE;
    }

    for (;;) {
        char enum_name[256];
        char unit_string[256];
        HKEY unit_key;
        char component_id_string[] = "ComponentId";
        char component_id[256];
        char net_cfg_instance_id_string[] = "NetCfgInstanceId";
        char net_cfg_instance_id[256];
        DWORD data_type;

        len = sizeof (enum_name);
        status = RegEnumKeyEx(
            netcard_key,
            i,
            enum_name,
            &len,
            NULL,
            NULL,
            NULL,
            NULL);

        if (status == ERROR_NO_MORE_ITEMS)
            break;
        else if (status != ERROR_SUCCESS) {
            return FALSE;
        }

        snprintf (unit_string, sizeof(unit_string), "%s\\%s",
                  ADAPTER_KEY, enum_name);

        status = RegOpenKeyEx(
            HKEY_LOCAL_MACHINE,
            unit_string,
            0,
            KEY_READ,
            &unit_key);

        if (status != ERROR_SUCCESS) {
            return FALSE;
        } else {
            len = sizeof (component_id);
            status = RegQueryValueEx(
                unit_key,
                component_id_string,
                NULL,
                &data_type,
                component_id,
                &len);

            if (!(status != ERROR_SUCCESS || data_type != REG_SZ)) {
                len = sizeof (net_cfg_instance_id);
                status = RegQueryValueEx(
                    unit_key,
                    net_cfg_instance_id_string,
                    NULL,
                    &data_type,
                    net_cfg_instance_id,
                    &len);

                if (status == ERROR_SUCCESS && data_type == REG_SZ) {
                    if (!strcmp (component_id, "tap0801") &&
                        !strcmp (net_cfg_instance_id, guid)) {
                        RegCloseKey (unit_key);
                        RegCloseKey (netcard_key);
                        return TRUE;
                    }
                }
            }
            RegCloseKey (unit_key);
        }
        ++i;
    }

    RegCloseKey (netcard_key);
    return FALSE;
}

static int get_device_guid(
    char *name,
    int name_size,
    char *actual_name,
    int actual_name_size)
{
    LONG status;
    HKEY control_net_key;
    DWORD len;
    int i = 0;
    int stop = 0;

    status = RegOpenKeyEx(
        HKEY_LOCAL_MACHINE,
        NETWORK_CONNECTIONS_KEY,
        0,
        KEY_READ,
        &control_net_key);

    if (status != ERROR_SUCCESS) {
        return -1;
    }

    while (!stop)
    {
        char enum_name[256];
        char connection_string[256];
        HKEY connection_key;
        char name_data[256];
        DWORD name_type;
        const char name_string[] = "Name";

        len = sizeof (enum_name);
        status = RegEnumKeyEx(
            control_net_key,
            i,
            enum_name,
            &len,
            NULL,
            NULL,
            NULL,
            NULL);

        if (status == ERROR_NO_MORE_ITEMS)
            break;
        else if (status != ERROR_SUCCESS) {
            return -1;
        }

        snprintf(connection_string,
             sizeof(connection_string),
             "%s\\%s\\Connection",
             NETWORK_CONNECTIONS_KEY, enum_name);

        status = RegOpenKeyEx(
            HKEY_LOCAL_MACHINE,
            connection_string,
            0,
            KEY_READ,
            &connection_key);

        if (status == ERROR_SUCCESS) {
            len = sizeof (name_data);
            status = RegQueryValueEx(
                connection_key,
                name_string,
                NULL,
                &name_type,
                name_data,
                &len);

            if (status != ERROR_SUCCESS || name_type != REG_SZ) {
                    return -1;
            }
            else {
                if (is_tap_win32_dev(enum_name)) {
                    snprintf(name, name_size, "%s", enum_name);
                    if (actual_name) {
                        if (strcmp(actual_name, "") != 0) {
                            if (strcmp(name_data, actual_name) != 0) {
                                RegCloseKey (connection_key);
                                ++i;
                                continue;
                            }
                        }
                        else {
                            snprintf(actual_name, actual_name_size, "%s", name_data);
                        }
                    }
                    stop = 1;
                }
            }

            RegCloseKey (connection_key);
        }
        ++i;
    }

    RegCloseKey (control_net_key);

    if (stop == 0)
        return -1;

    return 0;
}

static DWORD get_interface_index (const char *guid)
{
  ULONG index;
  DWORD status;
  wchar_t wbuf[256];
  snwprintf (wbuf, sizeof (wbuf), L"\\DEVICE\\TCPIP_%S", guid);
  wbuf [sizeof(wbuf) - 1] = 0;
  if ((status = GetAdapterIndex (wbuf, &index)) != NO_ERROR)
    return (DWORD)~0;
  else
    return index;
}

static int tap_win32_set_status(HANDLE handle, int status)
{
    unsigned long len = 0;

    return DeviceIoControl(handle, TAP_IOCTL_SET_MEDIA_STATUS,
                &status, sizeof (status),
                &status, sizeof (status), &len, NULL);
}

int open_tap(ip4_addr_t local_ip, ip4_addr_t local_mask, struct tap_info * ti)
{
    // #TODO this function needs some pretty heavy cleanup
    const char *prefered_name = NULL;
      
    int err;
    char device_path[256];
    char device_guid[0x100];
    int rc;
    HANDLE handle;
    DWORD index;
    ULONG ipapi_context;
    ULONG ipapi_instance;
    BOOL bret;
    char name_buffer[0x100] = {0, };
    struct {
        unsigned long major;
        unsigned long minor;
        unsigned long debug;
    } version;
    long len;

    if (prefered_name != NULL)
        snprintf(name_buffer, sizeof(name_buffer), "%s", prefered_name);

    rc = get_device_guid(device_guid, sizeof(device_guid), name_buffer, sizeof(name_buffer));
    if (rc)
        return -1;
    index = get_interface_index(device_guid);

    snprintf (device_path, sizeof(device_path), "%s%s%s",
              USERMODEDEVICEDIR,
              device_guid,
              TAPSUFFIX);

    handle = CreateFile (
        device_path,
        GENERIC_READ | GENERIC_WRITE,
        0,
        0,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_SYSTEM | FILE_FLAG_OVERLAPPED,
        0 );

    if (handle == INVALID_HANDLE_VALUE) {
        return -2;
    }

    bret = DeviceIoControl(handle, TAP_IOCTL_GET_VERSION,
                           &version, sizeof (version),
                           &version, sizeof (version), &len, NULL);

    if (bret == FALSE) {
        CloseHandle(handle);
        return -3;
    }
    
    if(!DeviceIoControl(handle, TAP_IOCTL_GET_MAC, ti->mac, 6, ti->mac, 6, &len, 0)) {
      return -4;
    }
    
    if ((err = AddIPAddress (local_ip,
                      local_mask,
                      index,
                      &ipapi_context,
                      &ipapi_instance)) != NO_ERROR)
    {
      printf("-5 Reason: %d\n", err);
      return -5;
    }
    
    if (!tap_win32_set_status(handle, TRUE)) {
        return -6;
    }
    
    ti->fd = _open_osfhandle ((long) handle, 0);
    
    if (ti->fd <0)
      return -10 + ti->fd;    
    
    return ti->fd;
}


void close_tap(int tap_fd)
{
   // #TODO we should have something here to delete the address that we added to the adapter
}

