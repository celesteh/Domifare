(

// CHECK the channel order!!

u = false;
h=false;

p = Pipe("aplay -l | grep -i ultra", "r");
protect {
	// line.isNil signals EOF
	// also rule out lines with "grep" (just in case)
	while { (l = p.getLine).notNil and: { l.contains("grep") } }
} { p.close };
// if jack server is not running, line will be nil
l.notNil.if({
	u = true;

	8.do({|i|
		8.do({|j|
			(i != j).if({
				"amixer -c Ultra set \"DIn% - Out%\" 0\% > /dev/null".format(i, j).unixCmd;
			} , {
				"amixer -c Ultra set \"DIn% - Out%\" 100\% > /dev/null".format(i, j).unixCmd;
			});
			"amixer -c Ultra set \"AIn% - Out%\" 0\% > /dev/null".format(i, j).unixCmd;
		});
	});
});

p = Pipe("aplay -l | grep -i h2", "r");
protect {
	// line.isNil signals EOF
	// also rule out lines with "grep" (just in case)
	while { (l = p.getLine).notNil and: { l.contains("grep") } }
} { p.close };
// if jack server is not running, line will be nil
l.notNil.if({
	h = true;
});


d = Domifare(s);
d.gui;
d.dEBUG = true;

s.waitForBoot({
	(u || h).if({
		// Don't use the internal mic
		"/usr/bin/jack_disconnect SuperCollider:in_1 Built-in\\\ Audio\\\ Analogue\\\ Stereo:capture_FL".unixCmd;
		"/usr/bin/jack_disconnect SuperCollider:in_2 Built-in\\\ Audio\\\ Analogue\\\ Stereo:capture_FR".unixCmd;
	});
	u.if({
		// Dont' use the built-in speakers
		"/usr/bin/jack_disconnect SuperCollider:out_1 Built-in\\\ Audio\\\ Analogue\\\ Stereo:playback_FL".unixCmd;
		"/usr/bin/jack_disconnect SuperCollider:out_2 Built-in\\\ Audio\\\ Analogue\\\ Stereo:playback_FR".unixCmd;
		// use the M-audio out
		"/usr/bin/jack_connect SuperCollider:out_1 M-Audio\\\ Fast\\\ Track\\\ Ultra\\\ Analogue\\\ Surround\\\ 7.1:playback_FL".unixCmd;
		"/usr/bin/jack_connect SuperCollider:out_2 M-Audio\\\ Fast\\\ Track\\\ Ultra\\\ Analogue\\\ Surround\\\ 7.1:playback_FR".unixCmd;
		// use the m-audio in
		"/usr/bin/jack_connect SuperCollider:in_1 M-Audio\\\ Fast\\\ Track\\\ Ultra\\\ Analogue\\\ Surround\\\ 7.1:capture_FL".unixCmd;
		"/usr/bin/jack_connect SuperCollider:in_2 M-Audio\\\ Fast\\\ Track\\\ Ultra\\\ Analogue\\\ Surround\\\ 7.1:capture_FR".unixCmd;
	});

	h.if({
		// use the H2 Zoom in
		"/usr/bin/jack_connect SuperCollider:in_1 H2\\\ Analogue\\\ Stereo:capture_FL".unixCmd;
		"/usr/bin/jack_connect SuperCollider:in_2 H2\\\ Analogue\\\ Stereo:capture_FR".unixCmd;
	});
});
)

// List of variables