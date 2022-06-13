(
d = Domifare(s);
d.gui;
d.dEBUG = true;

s.waitForBoot({
	"/usr/bin/jack_disconnect SuperCollider:in_1 Built-in\\\ Audio\\\ Analogue\\\ Stereo:capture_FL".unixCmd;
	"/usr/bin/jack_disconnect SuperCollider:in_2 Built-in\\\ Audio\\\ Analogue\\\ Stereo:capture_FR".unixCmd;
	"/usr/bin/jack_connect SuperCollider:in_1 H2\\\ Analogue\\\ Stereo:capture_FL".unixCmd;
	"/usr/bin/jack_connect SuperCollider:in_2 H2\\\ Analogue\\\ Stereo:capture_FR".unixCmd;
});
)

// Send freq when NOT changed
// X errors in a row automagically records into one of the 4 default vars.