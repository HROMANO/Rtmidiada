#include "rtmidi/rtmidi_c.h"

bool ada_rtmidi_ok(RtMidiPtr device) {
    return device->ok;
};

const char* ada_rtmidi_error_message(RtMidiPtr device) {
    return device->msg;
};
