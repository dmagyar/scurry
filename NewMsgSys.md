Scurry needs a new model for managing asynchronous messages.

# Introduction #

The current Scurry management messaging model isn't very good. We have one task generate messages on a time or need based model, but another task catches the responses and updates the state of the system. This produces at least two problems:
  * The code does not read as plainly as I would like
  * The program requires some sort of synchronizing state variable. Eventually I would like to remove all explicit state from the program (no IORef's or MVar's that are used for long term state storage).


# Delivery Guarantees #

Currently, management messages are not guaranteed to be delivered. There are many message types where this is definitely the desired behavior. Rather than having a separate tasks generate and look for these messages, it will be beneficial to have a single task manage the sending, retransmission, and response handling.