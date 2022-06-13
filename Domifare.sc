Domifare {

	var <syllables, <key, responder, <vars, <numbers, lastrecv, active, data,
	paused, syn, <>clock, gui, <thresh, <space, <longspace, <minspace,
	<recording, <server, <default_dur,<guiClass, default_var_names, var_maker,
	codeListeners, parser, statement, <>dEBUG, error_count, <>input_method, >liberalise;

	*initClass{

		StartUp.add {

			SynthDef(\recorder, { arg gate = 1, in=0, bufnum=0;

				var input, env, recorder;

				env = EnvGen.kr(Env.asr, gate, doneAction:2);
				input = SoundIn.ar(in, 1) * env;
				RecordBuf.ar(input, bufnum, loop:0, doneAction:2);

			}).writeDefFile;

			/*
			\instrument, \samplePlayer,
			\db, -3,
			\start_durs, [0, dur],
			\pan, 0.5.rrand(-0.5),
			\rate, 1,
			[\startFrame, \dur], Pfunc({|evt|
			evt[\start_durs]
			*/ // args amp, pan, rate, bufnum

			SynthDef(\samplePlayer, {|out=0, amp=0.2, pan=0, rate=1, bufnum=0, dur=1, startFrame=0|

				var env, player, panner;

				env = EnvGen.kr(Env([0, 1, 0], [0.1, dur, 0.2]), doneAction:2);
				player = PlayBuf.ar(1, bufnum, rate * BufRateScale.kr(bufnum), startPos:startFrame, loop:1);
				panner = Pan2.ar(player * env, pan, amp);

				Out.ar(out, panner);
			}).writeDefFile;

		}
	}



	*new {|server, clock, dur|

		^super.new.init(server, clock, dur);
	}


	init {|srv, clock, init_dur|

		var no_clock;

		dEBUG = true;
		error_count = 0;

		dEBUG.if({
			"init Domifare".postln;
		});

		// set guiClass
		guiClass = DomifareGui;
		statement = "";

		// inisitalise clocks

		default_dur = init_dur;
		input_method = false;
		liberalise = false;

		this.clock = clock;
		no_clock = this.clock.isNil;

		no_clock.if({
			this.clock = TempoClock(110/60, 16, queueSize:2048);
			this.clock.schedAbs(0, {
				this.clock.beatsPerBar_(16) // See below
			});
			// ok, but what if the duration disagrees?
			default_dur.notNil.if({
				this.clock.tempo = (default_dur / 16).reciprocal; // See above
			});
		});

		// if we don't have a default dur, base it off the clock

		default_dur.isNil.if({
			this.clock.schedAbs(0, {
				default_dur = this.clock.beatsPerBar * this.clock.beatDur;
			});
		}, {

			// if the clock and the dur disagree?
			(default_dur != (this.clock.beatsPerBar * this.clock.beatDur)).if ({
				// do we adjust the beat or the duration?
				// adjust the clock

				this.clock.tempo = (default_dur / this.clock.beatsPerBar).reciprocal;
			})
		});

		//no_clock.if({
		//	this.clock.schedAbs(0, {
		//		this.clock.beatsPerBar_(16) // See below
		//	});
		//});

		dEBUG.if({
			"default_dur is %".format(default_dur).postln;
		});

		syllables = [\Do, \Re, \Mi, \Fa, \Sol, \La, \Si];
		key = Key(Scale.major).change(chromatic: 3); // C maj

		//vars = (solfasire:nil, solfasisol:nil, soldosifa:nil);
		default_var_names = [\solfasire, \solfasisol, \soldosifa];
		vars = IdentityDictionary.new;
		numbers = (redodo: 1, remimi:2, refafa: 3, resolsol: 4, relala: 5, resisi: 6, mimido: 7, mimire:8);

		paused = true;
		recording = false;



		// Values used for parsing are passed to a synthdef and chageable via a GUI, so we're going an MVC thing
		//assemblemvc = {|ref, syntharg|
		//	var controller;
		//	controller = SimpleController(ref);
		//	controller.put(\value, {|theChanger, what, moreArgs|
		//		syn.notNil.if({
		//			syn.set(syntharg, theChanger.value);
		//		});
		//	});
		//	controller;
		//};


		minspace = Ref(0.1); // unref this
		//recording = Ref(false);
		//line = DomifareLine();

		/*
		// replace with command class below
		lang = (
		larelasi: [\larelasi, 2, 2, [\var, \data], nil], // func adds the name to the var array, runs the recorder
		dolamido: [\dolamido, 0, 1, [\var], nil], // func stops names loop or all loops
		domilado: [\domilado, 0, 1, [\var], nil], // func resumes named loop or all loops
		mifasol: [\mifasol, 0, 1, [\var], nil], // func raises an octave, which is probably impossible
		solfami: [\solfami, 0, 1, [\var], nil], // func lowers an octave- also impossible
		lamidore: [\lamidore, 2, 2, [\var, \data], nil], // add notes to existing loop
		dosolresi: [\dosolresi, 1, 1, [\var], nil], // shake the loop, which is possible with recordings also...
		misisifa: [\misisifa, 0, 1, [\var], nil], // next rhythm
		fasisimi: [\fasisimi, 0, 1, [\var], nil], //previous rhythm
		misoldola: [\misoldola, 0, 1, [\var], nil], //random rhytm
		refamido: [\refamido, 0, 0, [], nil], // die
		sifala: [\sifala, 2, 2, [\number, \operator], nil], // repeat N times (1x/bar)
		larefami: [\larefami, 2, 2, [\number, \operator], nil] // X in 8 chance of doing the command
		);
		*/

		//name, minargs, maxargs, types, func, launcher
		dEBUG.if({
			"define larelasi".postln;
		});


		var_maker = DomifareCommand.define(\larelasi, 1, 1, [\varname], {|lang, varname|  // declare loop
			var loop, action;
			"declare loop".postln;
			loop = DomifareLoop(varname, this.default_dur); // ok, here's our new loop
			// add a variable
			lang.vars.put(varname.asSymbol, loop); // new DomiFareLoop

			// record into it
			// the recording method handles pausing "text" input
			this.pr_record(loop, recordAction: {
				loop.play(this.clock);
			});


		}, this);
		DomifareCommand.define(\dolamido, 0, 1, [\var], {|lang, loop|
			// stop loop
			lang.stop(loop);
		}, this);
		DomifareCommand.define(\domilado, 0, 1, [\var], {|lang, loop|
			// start loop
			lang.start(loop);
		}, this);
		DomifareCommand.define(\lamidore, 1, 1, [\var], {|lang, loop|
			// add notes to loop
			//lang.pause;
			//loop.addNotes;
			//lang.resume;
		}, this);
		DomifareCommand.define(\misisifa, 0, 1, [\var], {|lang, loop|
			lang.next(loop);
		}, this);
		DomifareCommand.define(\fasisimi, 0, 1, [\var], {|lang, loop|
			lang.prev(loop);
		}, this);
		DomifareCommand.define(\misoldola, 0, 1, [\var], {|lang, loop|
			lang.rand(loop);
		}, this);
		//DomifareCommand.define(\refamido, 0, 0, [], {}); // Just stop all loops instead?
		DomifareCommand.define(\sifala, 3, 3, [\number, \number, \operator], {|lang, x, y, cmd|
			// this is complicated
		}, this); // X times every Y bars
		DomifareCommand.define(\larefami, 2, 2, [\number, \operator], {|lang, x, cmd|
			var guess;
			guess = 8.rand + 1;
			(guess < x ).if({ // double check this logic
				cmd.eval.value
			});
		}, this);



		lastrecv = 0;

		srv.isNil.if({
			srv = Server.default;
		});
		this.server_(srv);

		//this.gui;

	}

	stop {|loop|
		// loop is nil stop everything
		// loop is symbol, map to var
		// loop is var, stop that loop
		this.pr_call(loop, 'stop');
	}

	play {|loop|
		// loop is nil start everything
		// loop is symbol, map to var
		// loop is var, start that loop
		this.pr_call(loop, 'play');
	}

	next {|loop|
		// loop is nil start everything
		// loop is symbol, map to var
		// loop is var, start that loop
		this.pr_call(loop, 'next');
	}

	prev {|loop|
		// loop is nil start everything
		// loop is symbol, map to var
		// loop is var, start that loop
		this.pr_call(loop, 'prev');
	}

	rand {|loop|
		// loop is nil start everything
		// loop is symbol, map to var
		// loop is var, start that loop
		this.pr_call(loop, 'rand');
	}

	shake {|loop|
		// loop is nil start everything
		// loop is symbol, map to var
		// loop is var, start that loop
		this.pr_call(loop, 'shake');
	}

	pr_call{|loop, message|
		loop.isNil.if({ // pass the message to everything if loop is nil
			vars.do({|item|
				Message(item, message.asSymbol);
			});
		} , {
			// otherwise, pass the message to the appropriate loop
			loop.isKindOf(DomifareLoop).not({
				loop = vars[loop.asSymbol];
			});
			loop.notNil.if({
				Message(loop, message.asSymbol)
			})
		});
	}


	pause{
		dEBUG.if({
			"\npausing".postln;
		});

		paused= true;
		//collectOnsets = false;
		parser.pause = paused;
	}

	resume{
		dEBUG.if({
			"\ngoing".postln;
		});

		paused = false;
		//collectOnsets = true;
		//onsets = [];
		parser.pause = paused;
	}



	pr_record {|loop, buffer, dur, pad=0.01, in=0, clock, bufferAction, recordAction|

		var loop_dur, numFrames;
		// if buffer is a number, then it's a bufnum;

		loop_dur = dur ? loop.dur? this.default_dur.value; // if the passed in dur is nil, use the default

		loop.dur = loop_dur;

		buffer = buffer ? loop.buffer;
		clock = clock ? this.clock;

		numFrames = server.sampleRate * (loop_dur + pad);

		// fork this because it requires syncing
		{
			buffer.isNil.if({
				buffer = Buffer.alloc(server, numFrames, completionMessage:{|buffer|
					loop.buffer = buffer;
					bufferAction.value(buffer);
					[]
				});
				server.sync;
			});
			Pseq([
				Pbind(
					\instrument, \recorder,
					\in, in,
					\buffer, buffer,
					\dur, loop_dur,
					\is_recording, Prout({
						recording = true;
						//onsetFunc = 1; // start geting onsets
						//onsets = [];
						parser.pause = true;
						this.changed;
						true.yield; // ok, let's go!
						// don't yield a second time. The nil stops the recording from repeating
					})
				),
				Pbind(
					\instrument, Pfunc({
						recording = false;
						loop.offsets = parser.onsets.copy; // grab these
						//onsets = [];
						//onsetFunc = nil; // stop adding to it
						parser.pause = false;
						this.changed; // tell the GUI
						recordAction.value(loop);

						nil;
					})
				)
			], 1).play(clock);

		}.fork;
	}

	default_dur_{|newdur, theChanger|

		default_dur = newdur;
		this.changed(theChanger);
	}

	update {|theModel, theChanger| // this may cause an infinite loop

		((theChanger != this) && (theModel != this)).if({
			this.changed(theChanger, theModel); // use the moreArgs
			// to say what changed
		});
	}


	server_{|srv|

		server = srv;

		server.waitForBoot({


			thresh = Bus.control(server, 1).set(0.2);
			space = Bus.control(server, 1).set(0.5);
			longspace = Bus.control(server, 1).set(2);

			server.sync;

			parser = DomifareParser(this, server);

			this.changed(this, this);


		});
	}

	gui {|parent, bounds|
		var gui;
		//guiClass = this.guiClass;
		gui = guiClass.new( this );
		gui.gui( parent, bounds );
		//ui = super.gui(this);
		this.changed(this, this);
		//line.addDependant(gui);
		gui.welcome;
		^gui;
	}

	registerCodeListener {|listener|
		codeListeners.isNil.if({
			codeListeners = [listener];
		}, {
			codeListeners.indexOf(listener).isNil.if({
				codeListeners = codeListeners.add(listener);
			})
		})
	}

	removeCodeListener {|listener|
		var index;

		index = codeListeners.indexOf(listener);
		index.notNil.if({
			codeListeners.removeAt(index);
		})
	}

	code_updated {|text|

		dEBUG.if({
			text.post;
		});

		statement = statement ++ text;

		codeListeners.do({|listener|
			listener.code_updated(text);
		})
	}

	code_executed {|text|
		dEBUG.if({
			text.postln;
		});

		codeListeners.do({|listener|
			listener.code_execute(statement ++ text);
		});
		statement = "";
		this.showpersonship(false);
	}

	error_handler{|err|
		codeListeners.do({|listener|
			listener.code_error(err);
		});
		//err.throw;
		//"".postln;
		//err.postln;
		"\nERROR: %".format(err.errorString).postln;
		statement = "";
		//this.showpersonship(true); // disabling error counting
	}

	token_{|word|

		var err, data, result, token;

		token = word.toLower;
		active.value.isNil.if({ // start of a new command

			active = DomifareCommand(token);
			active.value.isNil.if({
				// We have not entered a new command,
				// but if it's 4 letters, maybe it's a new loop
				(liberalise && "sol".matchRegexp(token.asString, 0, 3)).if({ // does it start with sol?
					((active.size == 9) || (active.size ==10)).if({
						// record
						"Going to record".postln;
						active = var_maker;//DomifareCommand(\larelasi);
						result = active.var_(token);
						result.isKindOf(Function).if({
							result.value();
							this.code_executed(var_maker.name.asString + token);
							active = nil;
						})
					});
				} , {
					err = CommandNotFound("Command not found: %.".format(token));
					this.error_handler(err);
				});
			}, {"new command %".format(token).postln;});

			// We've either started a new line or thrown an error
		},{
			// Already in he middle of a command

			// Could this token be a number?
			data = numbers[token.asSymbol];

			data.isNil.if({
				// Could this token be a var?
				data = vars[token.asSymbol];

				data.isNil.if({
					// the token string is the data
					data = token;
				});
			});

			result = active.var_(data);
			result.isKindOf(Error).if({
				this.error_handler(result);
				// this command is done
				active = nil;
			});

			// Is the command done?
			(result == true).if({ //
				// the command is done
				active = nil;
			},{
				(result.isKindOf(Function)).if({
					result.value;
					this.code_executed(active.name.asString + word);
					// the command is done
					active = nil;
				})
			});

		});


	}

	eol{
		var result;

		active.notNil.if({
			// execute the command
			result = active.eval;
			result.notNil.if({
				result.isKindOf(Error).if({
					this.error_handler(result);
				}, {
					result.value;
				});
			});

			this.code_executed();

			// this command is done
			active = nil;
		});

	}

	requireOnsets_{|bool|
		parser.requireOnsets = bool;
	}

	showpersonship {|erred = true|
		var varname, free;

		erred.not.if({
			error_count = 0;
		} , {

			(error_count  > 10.pow(vars.size +1)).if({

				varname = default_var_names.pop;
				free = false;
				{varname.notNil && free.not}.while ({
					free = vars[varname].isNil;
				});

				varname.notNil.if({
					this.code_executed("% Errors in a row.\nRecording into %".format(error_count, varname));
					active = var_maker; //DomifareCommand(\larelasi);
					active.var_(varname);
					//result = active.eval();
					this.eol;
				});
				error_count = 0;

			}, {
				error_count = error_count +1;
			});
		});
	}

}

// Parsing is too cimplicated, so it gets it's own object
DomifareParser {
	var lang, codeListeners, paused, <>onsets, /*lastOnset, */ pitches,
	/*syllables, key,*/ responder, <token, lastrecvtime, >requireOnsets,
	space, pitchSemaphore, <server, <syn, lastrecv;

	*initClass{

		StartUp.add {
			SynthDef(\domifare_input, { arg gate=1, in=0, thresh=0.2, space=0.5, longspace=2,
				rmswindow = 1600;

				var input, env, fft_pitch, onset, chain, hasfreq, paused, linebreak,
				rms, xings, peaks, freq, fgate, ftrig, changed;

				input = SoundIn.ar(in, 1);
				env = EnvGen.kr(Env.asr, gate, doneAction:2);

				chain = FFT(LocalBuf(2048), input);
				onset = Onsets.kr(chain, odftype:\phase);//odftype:\wphase);
				#fft_pitch, hasfreq = Pitch.kr(input, maxFreq:300, ampThreshold:thresh);
				paused = DetectSilence.ar(input, thresh, space);
				linebreak = DetectSilence.ar(input, thresh, longspace);

				// time donaim
				rms = (RunningSum.ar(input.squared, rmswindow)/rmswindow).sqrt;
				peaks = input - rms;
				xings = ZeroCrossing.ar(peaks);
				freq = xings /2;

				//send pitch
				SendTrig.kr(hasfreq, 0, fft_pitch);

				// triggering/gating time domain pitch
				//fgate = A2K.kr(EnvFollow.ar(input));
				fgate = (A2K.kr(paused) - 1 ).abs;
				// when paused is 1, paused -1 is zero, else, non zero

				// time domain pitch - send after the onset
				//ftrig = Impulse.kr(10/60);
				//hasfreq + (TDelay.kr(onset, 0.1));// + fgate;
				changed = A2K.kr(Changed.ar(freq, 5));
				ftrig = changed.not;
				//freq = freq * fgate;
				//SendTrig.kr(hasfreq * fgate, 4, freq);
				//SendTrig.kr(TDelay.kr(onset, 0.05) * fgate, 4, freq);
				SendTrig.kr(ftrig * fgate, 4, freq);
				//SentTrig.kr(TDelay.kr(changed, 0.05), 4, freq);


				// send onsets
				SendTrig.kr(onset, 1, 1);

				// send silence
				SendTrig.kr(A2K.kr(paused), 2, 1);

				// long silence (End of line)
				SendTrig.kr(A2K.kr(linebreak), 3, 1);

			}).writeDefFile;
		}
	}

	*new {|lang, server|
		^super.new.init(lang, server);
	}

	init {|theLang, server|

		lang = theLang;
		lang.addDependant(this);

		space = 0;
		this.registerCodeListener(lang);

		pitches = [];
		pitchSemaphore = Semaphore(1);

		lastrecv = 0;
		paused  = true;
		token = "";
		onsets = [];
		requireOnsets = false;

		this.server_(server);

	}

	code_updated {|text|
		lang.dEBUG.if({
			text.post;
		});

		codeListeners.do({|listener|
			listener.code_updated(text);
		})
	}

	registerCodeListener {|listener|
		codeListeners.isNil.if({
			codeListeners = [listener];
		}, {
			codeListeners.indexOf(listener).isNil.if({
				codeListeners = codeListeners.add(listener);
			})
		})
	}

	removeCodeListener {|listener|
		var index;

		index = codeListeners.indexOf(listener);
		index.notNil.if({
			codeListeners.removeAt(index);
		})
	}

	pause_{|shouldPause|
		paused = shouldPause;
		// whatever's happening, clear the onsets
		onsets = [];
	}

	server_{|srv|

		var semaphore;

		semaphore = Semaphore(1);

		server = srv;

		server.waitForBoot({

			lang.space.get({|value| space = value;});

			// PUT THIS IN A GUI
			syn = Synth(\domifare_input,
				[\gate, 1, \in, 0, \thresh, lang.thresh.asMap, \space, lang.space.asMap,
					\longspace, lang.longspace.asMap, \rmswindow, 200],server);

			// out with the old (if it exists)
			OSCdef(\domifare_in).free;



			// This needs re-writing to deal with the command class

			OSCdef(\domifare_in, {|msg, time, addr, recvPort|
				var tag, node, id, value, letter, result, err;

				semaphore.wait;

				#tag, node, id, value = msg;
				//[tag, id, value].postln;
				case
				{ id ==0 } { /* pitch */
					"pitch".postln;

					//"not recording".postln;
					paused.not.if({
						lang.dEBUG.if({
							"autocorrelation %".format(value).postln;
						});
						lang.input_method.if ({
							this.freq_(value, time);
						},{"not auto".postln;});
					});

				}
				{ id ==1 } { /* onset */
					this.received_onset = time;

				}
				{ id ==2 } { /* space */

					paused.not.if({

						lang.dEBUG.if({ "space".postln });

						this.received_space = time;


					});

				}
				{ id ==3 } { /* EOL */

					paused.not.if({
						lang.dEBUG.if({ "eol".postln; });
						lang.eol;

					});
				}
				{ id == 4 } { /* time donaim */
					paused.not.if({

						value = value.asInteger.abs;
						((value > 0) && (value < 300)).if ({
							lang.dEBUG.if({
								"time domain %".format(value).postln;
							});
							lang.input_method.not.if({
								this.freq_(value, time);
							},{"not time".postln;});
						});
					});
				};

				semaphore.signal;

			}, '/tr', server.addr);
		});



	}


	best_letter{|time, space=0|

		var end, count, durs, last, num, time_tuple, dur,
		occurances, winner, pcopy;

		// candidates are stored [num, time]
		end = time-space;
		last = end;
		count = Array.fill(lang.syllables.size, {0});
		durs = Array.fill(lang.syllables.size, {0});

		// get a lock on the pitches
		pitchSemaphore.wait;
		pcopy = pitches.reverse;
		pitches=[];
		pitchSemaphore.signal;

		winner = nil;

		(pcopy.size > 0).if({

			// first, sum all the time for each note
			pcopy.do({|tuple, index|
				num = tuple.first;
				time_tuple = tuple.last;
				dur = (last-time_tuple).abs;
				durs[num] = durs[num] + dur;
				count[num] = count[num] + 1;
				last = time_tuple;
			});

			// then, find the one with the most occurances
			occurances = 0;
			count.do({|item, index|
				(item > count[occurances]).if({
					occurances = index;
				})
			});

			// now find the one with the greatest duration
			winner = 0;
			durs.do({|item, index|
				(item> count[winner]).if({
					winner = index;
				})
			});

			// How often do these mismatch???
			(winner != occurances).if({
				"WARNING: Ambigious note - % or %"
				.format(lang.syllables[occurances], lang.syllables[winner])
				.postln;
			});

			//winner = lang.syllables[winner];

			this.code_updated(lang.syllables[winner]);

		});

		^winner;
	}

	received_letter_{|letter, number, time|
		"got letter".postln;
		requireOnsets.if({
			// candidates are stored [num, time]
			pitchSemaphore.wait;
			pitches = pitches.add([number, time]);
			pitchSemaphore.signal;
		},{
			((time - lastrecv).abs > lang.minspace.value).if({
				// ignore things that follow too close on
				token = (token ++ letter);
				lang.dEBUG.if({
					token.postln;
				});

				//line.append(letter).changed(\append);
				// alert listeners
				this.code_updated(letter);
				//word.postln;
				lastrecv = time;
			});
		})

	}



	received_space_{|time|
		var last_letter;

		// let's just update space here
		//lang.space.get({|value| space = value;});

		this.code_updated(" ");

		// figure out what letter we just had

		requireOnsets.if({
			last_letter = this.best_letter(time, space);
			last_letter.notNil.if({
				last_letter = lang.syllables.wrapAt(last_letter);
				token = token ++ last_letter;
			});
		});

		// make sure the token isn't empty
		token = token.stripWhiteSpace;
		(token.size > 0).if ({
			lang.token_(token);
		});
		token = "";


	}

	received_onset_{|time|

		var last_letter;

		// figure out what letter we just had
		paused.not.if ({
			requireOnsets.if({
				last_letter = this.best_letter(time, space);
				last_letter.notNil.if({
					last_letter = lang.syllables.wrapAt(last_letter);
					token = token ++ last_letter;
				});
			});
		});

		onsets = onsets.add(time);

	}

	freq_{|freq, time|
		var letter, num;

		num = lang.key.freqToDegree(freq.asInteger.abs);
		num = num % lang.syllables.size;
		letter = lang.syllables.wrapAt(num);

		this.received_letter_(letter, num, time);
	}

	update {
		// let's just update space here
		lang.space.get({|value| space = value;});
	}


}

DomifareError : Error {}

WrongType : DomifareError {}

CommandNotFound : DomifareError {}

NoSuchVariable : DomifareError {}


DomifareGui : ObjectGui {

	var history, activeLine, sliders, rec_button, run_button, textView, lineView, font, onsetButton;

	*new {|model|
		^super.new(model).init;
	}

	init {
		model.registerCodeListener(this);
	}

	guiBody { arg view;

		var index;

		//super.guiBody(view);
		model.dEBUG.if({
			"in guiBody".postln;
		});

		sliders = IdentityDictionary.new();

		model.dEBUG.if({
			view.class.postln;
		});
		//view.bounds = Rect(0,0, 1000, 1000);

		run_button = Button(view, 90@25).states_([
			["Run ⏯"], ["Pause ⏸"]
		]).action_({|but|
			(but.value == 1).if({
				// play
				model.resume;
			}, {
				// pause
				model.pause;
			});
		});

		onsetButton = Button(view, 130@25).states_([
			["Require Onsets"], ["Onsets Required"]
		]).action_({|but|
			(but.value == 1).if({
				// we want to require onsets
				model.requireOnsets = true;
			}, {
				model.requireOnsets = false;
			});
		});

		// input method button
		Button(view, 150@25).states_([
			["Frequency Domain"],["Autocorrelation"]
		]). action_({|but|
			(but.value== 1).if ({
				model.input_method = true;
				"switched inputs".postln;
			}, {
				model.input_method = false;
				"switched inputs".postln;
			})
		});

		Button(view, 90@25).states_([
			["Strict"], ["Liberal"]
		]). action_({|but|
			(but.value == 1).if({
				model.liberalise = true;
				"more liberal".postln;
			}, {
				model.liberalise = false;
				"less liberal".postln;
			});
		});


		rec_button = Button(view, 40@25).states_([
			[" ", Color.black, Color.clear],
			["R", Color.black, Color.red]]);

		//view.startRow;

		view.startRow;
		ServerMeterView.new(model.server, view, 0@0, 1, 0).view.resize_(4); // left
		view.flow({|flow|
			var width;

			width = view.innerBounds.width - (ServerMeterView.getWidth(1,0) + ServerMeterView.getWidth(0,2)+100);
			flow.startRow;
			font = Font("Courier",18);
			textView = TextView(flow, //bounds
				Rect(0,0,width,(ServerMeterView.height *2)-10)
			).autohidesScrollers_(false).font_(font).resize_(5); // stretch
			textView.background = Color.black;
			textView.setStringColor(Color.white); textView.stringColor_(Color.white);
			flow.startRow;

			lineView = EZText(flow, Rect(0,0,width, 30), "Input: ").font_(font);
			lineView.textField.value="";
			lineView.setColors(Color.grey,Color.white,background: Color.grey(0.7));
			lineView = lineView.textField;
			lineView.resize(8);

			flow.resize_(5);
		});
		ServerMeterView.new(model.server, view, 0@0, 0, 2).view.resize_(6); // right

		view.startRow;



		index = \thresh;
		sliders[index] = EZSlider(view, ((view.innerBounds.width/2)-50)@20, index, \db,  {|ez|
			model.thresh.set(ez.value.dbamp);
		});
		sliders[index].sliderView.resize_(8);

		index = \space;
		sliders[index] = EZSlider(view, ((view.innerBounds.width/2)-50)@20, index, ControlSpec(0.1, 2, 0.1),
			{|ez|
				model.space.set(ez.value);
				model.changed(this, model.space);
		});
		sliders[index].sliderView.resize_(8);

		view.startRow;

		index = \longspace;
		sliders[index] = EZSlider(view, ((view.innerBounds.width/2)-50)@20, index, ControlSpec(1, 5, 1),
			{|ez|
				model.longspace.set(ez.value.dbamp);
		});
		sliders[index].sliderView.resize_(8);

		index = \minspace;
		sliders[index] = EZSlider(view, ((view.innerBounds.width/2)-50)@20, index, ControlSpec(0.01, 0.2, 0.1),
			{|ez|
				model.minspace.set(ez.value);
		});
		sliders[index].sliderView.resize_(8);

		//textView.bounds = Rect(0,0,
		//	view.innerBounds - (ServerMeterView.getWidth(1,0) + ServerMeterView.getWidth(0,2)),
		//	ServerMeterView.height);


		//view.layout = VLayout(
		//	Button(view),
		//threshslider.view,
		//spaceslider.view,
		//	Button(view)
		//)

		//this.update;
		//view.scroll(autohidesScrollers:false, autoScrolls:false,
		//	hasHorizontalScroller: false, hasVerticalScroller: false);

		//view.parent.scroll(autohidesScrollers:false, autoScrolls:false,
		//	hasHorizontalScroller: false, hasVerticalScroller: false);
		view.reflowDeep;
		view.resize_(5);


	}


	update { |theModel, theChanger|

		//theModel.class.postln;


		(theChanger != this).if({
			model.dEBUG.if({
				"updating".postln;
			});
			/*
			theModel.isKindOf(DomifareLine).if({

			theModel.postln;
			theChanger.postln;

			}, { // else normal model
			*/
			AppClock.sched(0, {
				// are we recording?
				model.recording.value.if({
					rec_button.value = 1;
				},{
					rec_button.value=0;
				});

				// just grab the value
				sliders[\minspace].value = model.minspace.value;

			});

			// get the busses
			model.thresh.get({|value| //"thresh %".format(value).postln;
				AppClock.sched(0.0,{sliders[\thresh].value = value.value.ampdb})});
			model.space.get({|value|
				AppClock.sched(0.0,{sliders[\space].value = value.value})});
			model.longspace.get({|value|
				AppClock.sched(0.0,{sliders[\longspace].value = value.value})});

			//})
		});
	}

	append{|text, colorise = false|
		var size, start;

		size = text.size;

		AppClock.sched(0.0,{
			start = textView.string.size -2; // found via trial and error
			textView.string = textView.string ++ text ++ "\n";
			// fucking stupid shit to fix the scrolling issue
			textView.select(textView.string.size+1, 0);
			textView.stringColor_(Color.white);
			textView.setFont(font);
			//textView.syntaxColorize;
			colorise.if({
				textView.setStringColor(Color.red, start, size);
			});
		});
	}

	code_updated{|text|

		AppClock.sched(0.0,{lineView.string = lineView.string ++ text;});
	}

	code_error{|err|
		var text, start, size;

		text = /*"ERROR:" + */ "ReSolRe:" + err.errorString;
		size = text.size;

		this.append(text, true);

		AppClock.sched(0.0,{
			lineView.value = "";
		});

		//err.throw();
		//"".postln;
		//err.postln;
	}

	code_execute{|text|

		this.append(text);

		AppClock.sched(0.0,{
			lineView.value = "";
		});
	}

	welcome {|dur=15|
		var text, pause;

		model.dEBUG.if({
			"In welcome".postln;
		});

		text = [
			"DoLaDoRe Fa DoMiFaRe",
			"Welcome to DoMiFaRe",
			"SiFaSiRe SiSolReDo SolReDoLa Fa ReMiLa SiSiDoMi MiFaRe",
			"Please play tuba to enter code",
			"LaReLaSi - define variable",
			"DoSolReSi - reorder notes",
			"DoLaMiDo - stop",
			"DoMiLaDo - resume",
			"LaLaMiDo ⏯ fa sidofa",
			"Press Run to Start",
			"------------------"
		];

		pause = dur / text.size;

		{
			model.dEBUG.if({
				"Forked dur % lines %".format(pause, text.size).postln;
				text.postln;
			});
			text.do({|phrase|
				pause.wait;
				this.append(phrase);
				model.dEBUG.if({
					//"loop".postln;
					phrase.postln;
				});
			});
		}.fork
	}




}





DomifareCommand {

	classvar dict;
	var <name, <minargs, <maxargs, types, >func, launcher, vars, subcommand;

	*initClass {
		dict = IdentityDictionary.new
	}



	*define{|name, minargs, maxargs, types, func, launcher|
		var command;
		"new command".postln;
		name = name.toLower;
		command = dict[name.asSymbol];
		command.isNil.if({
			"new command coined".postln;
			minargs.notNil.if({ // don't create a new one for nil values
				command = super.newCopyArgs(name, minargs, maxargs, types, func, launcher);
				dict.put(name.asSymbol, command);
			});
		});
		^command;
	}

	*new{|name|
		var command;
		name = name.toLower;
		command = dict[name.asSymbol];
		^command;
	}


	var_{|newvar|
		var newindex, type, ret;
		vars.isNil.if({
			vars = [];
		});

		// if we've got sub commands going, pass the args down
		subcommand.notNil.if({
			ret = subcommand.var_(newvar);
		} , {
			newindex = vars.size; // what??
			type = types[newindex];
			// check the types
			case
			{ (type == \var) || (type == DomifareLoop) } {
				newvar.isKindOf(DomifareLoop).not.if({
					ret = WrongType("Variable is wrong type.");
				}, {
					vars = vars ++ newvar;
				});
			}
			{ (type == \varname) || (type == DomifareLoop) } {
				(newvar.isKindOf(String).not &&
					newvar.isKindOf(Symbol).not).if({
					ret = WrongType("Variable is wrong type.");
				}, {
					vars = vars ++ newvar;
				});
			}
			{ ( type == \number) || (type == SimpleNumber) } {
				newvar.isKindOf(SimpleNumber).not.if({
					ret = WrongType("Variable is wrong type.");
				}, {
					vars = vars ++ newvar;
				});
			}
			{ ( type == \operator) || (type == DomifareCommand) } {
				(newvar.isKindOf(Symbol) || newvar.isKindOf(String)).if({
					newvar = DomifareCommand(newvar.asSymbol);
				});
				newvar.notNil.if({
					newvar.isKindOf(DomifareCommand).not.if({
						ret = NotFound("command not found.");
					}, {
						subcommand = newvar;
						ret = false;
					});
				}, {
					ret = NotFound("command not found.");
				});
			};
			// ret is nil if we haven't figured out what to do yet
			ret.isNil.if({
				// if we've got all the args, we can eval now
				(vars.size == maxargs).if ({
					// evaluate immediately
					ret = this.eval
				} , {
					// or else we're not done
					ret = false;
				});
			});
		});

		^ret;

	}

	eval {
		// should return a function to support sub commands
		^{
			func.value(launcher, *vars);
		}

	}

	clear {
		subcommand = nil;
		vars = [];
	}

	done {
		this.clear
	}

}


// Start with a simple loop class and make it more interesting in a later version

DomifareLoop {

	var <name, <>dur, <buffer, <offsets, <durs, <start_durs, isplay;

	*new {|name, dur, buffer, offsets|
		^super.newCopyArgs(name.asSymbol, dur).init(offsets, buffer);
	}

	init {|offsets, buffer|
		//var pdef;

		this.offsets_(offsets);

		//pdef = Pdef(name);
		//pdef.source.isNil.if({
		Pdef(name,
			Pbind(
				\instrument, \samplePlayer,
				\bufnum, -1,
				\db, -3,
				\start_durs, [0, dur],
				\pan, 0.5.rrand(-0.5),
				\rate, 1,
				[\startFrame, \dur], Pfunc({|evt|
					evt[\start_durs]
				}),
				\status, Pfunc({|evt| "playing loop % %".format(name, evt[\dur]).postln})
			)
		);
		//})
		buffer.notNil.if({
			this.buffer_(buffer);
		});
	}


	pbindef {
		^Pbindef(name);
	}

	pdef {
		^Pdef(name);
	}

	buffer_ { |buf|
		var pd;

		buffer = buf;

		pd = this.pbindef();
		pd.notNil.if({
			Pbindef(name,
				\bufnum, buffer
			);
		});
	}

	offsets_{|offs|

		// passing in an array of offet times can be used to calculate durations
		var diffs, last, actionable=true;

		offs.isNil.if({
			start_durs = [0, dur];
		}, {
			(offs.size < 1).if({
				start_durs = [0, dur];
			} , {

				diffs = offs.differentiate; // diffs[1..n] has the first n-1 durs
				diffs.removeAt(0); // the first item is unchanged by differentiate
				durs = diffs ++ (dur - diffs.sum); // calculate the final duration
				offsets = offs - offs.first; // when does each event start?

				// now pair them
				start_durs = offsets.collect({|item, index|
					[item, durs[index]]
				});
			});
		});
	}



	play {|clock, quant ...args|

		var pd, isExt = false;

		pd = this.pdef();

		// Compatibility with BBCut
		\ExternalClock.asClass.notNil.if({
			isExt = clock.isKindOf(ExternalClock);
		});

		pd.notNil.if({

			isExt.if({ // this also checks for nil
				isplay.isNil.if({ isplay = false });
				isplay.not.if({
					//"external".postln;
					pd.playExt(clock, nil, quant);
					isplay = true;
				});
			} , {
				"tempo".postln;
				pd.dump;
				//pd.isPlaying.not.if({
				pd.play(clock, nil, quant);
				//});
			});
		})

	}

	stop {|...args|
		var pd;


		pd = this.pbindef();
		pd.notNil.if({pd.stop();});
		isplay.notNil.if({ isplay = false});
	}


	pause {
		var pd;

		pd = this.pbindef();
		pd.notNil.if({pd.pause();})

	}

	shake {
		var pd;
		//var indexes, offs, ds, pd;

		// we have to scamble the offsets and durations both together
		// so scramble the indexes
		//indexes = (0..(offsets.size-1)).scramble;

		// now reassemble offsets and durs
		//offs = indexes.collect({|i|
		//	offsets[i];
		//});

		//ds = indexes.collect({|i|
		//	durs[i];
		//});

		// we could do semaphors to be sure not to clobber these values mid loop,
		// but that's too much trouble;

		//pairs = indexes.collect({|item, i|
		//	[offsets[i], durs[i]]
		//});

		//offsets = offs;
		//durs = ds;

		pd = this.pbindef();
		pd.notNil.if({
			Pbindef(name,
				\start_durs, Pshuf(start_durs, inf)
			);
			Pdef(name).quant = dur; // The first shake doesn't need this but subsequent ones do
		});
	}

	unshake {
		var pd;

		pd = this.pbindef();
		pd.notNil.if({
			Pbindef(name,
				\start_durs, [0, dur]
			);
			Pdef(name).quant = dur; // The first shake doesn't need this but subsequent ones do
		});
	}

	xshakey {|x, y| // x times, every Y bars

		var pd, shufs;

		shufs = x-1;

		pd = this.pbindef();
		pd.notNil.if({
			Pbindef(name,
				\start_durs, Pseq([Pn(Pshuf(start_durs, y), shufs), Pshuf(start_durs, inf)], inf)
			);
			Pdef(name).quant = dur; // The first shake doesn't need this but subsequent ones do
		});

	}





}
