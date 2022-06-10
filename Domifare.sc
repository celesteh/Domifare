Domifare {

	var syllables, key, responder, <vars, <numbers, word, lastrecv, active, data, >onsetFunc, <>onsets,
	paused, syn, <>clock, gui, <thresh, <space, <longspace, <minspace, <threshc, <spacec, <longspacec,
	<minspacec, <line, <linec, <recording, <server, <default_dur,<guiClass, codeListeners;

	*initClass{

		StartUp.add {
			SynthDef(\domifare_input, { arg gate=1, in=0, thresh=0.2, space=0.5, longspace=2;

				var input, env, fft_pitch, onset, chain, hasfreq, paused, linebreak;

				input = SoundIn.ar(in, 1);
				env = EnvGen.kr(Env.asr, gate, doneAction:2);

				chain = FFT(LocalBuf(2048), input);
				onset = Onsets.kr(chain, odftype:\phase);//odftype:\wphase);
				#fft_pitch, hasfreq = Pitch.kr(input, ampThreshold:thresh);
				paused = DetectSilence.ar(input, thresh, space);
				linebreak = DetectSilence.ar(input, thresh, longspace);

				//send pitch
				SendTrig.kr(hasfreq, 0, fft_pitch);

				// send onsets
				SendTrig.kr(onset, 1, 1);

				// send silence
				SendTrig.kr(A2K.kr(paused), 2, 1);

				// long silence (End of line)
				SendTrig.kr(A2K.kr(linebreak), 3, 1);

			}).writeDefFile;

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

		var assemblemvc;

		"init Domifare".postln;

		// set guiClass
		guiClass = DomifareGui;

		// inisitalise clocks

		default_dur = init_dur;

		this.clock = clock;
		this.clock.isNil.if({
			this.clock = TempoClock(110/60, 16, queueSize:2048);
			this.clock.schedAbs(0, {
				this.clock.beatsPerBar_(16) // See below
			});
			// ok, but what if the duration disagrees?
			default_dur.notNil.if({
				this.clock.beatDur = default_dur / 16; // See above
			});
		});

		// if we don't have a default dur, base it off the clock

		default_dur.isNil.if({
			default_dur = this.clock.beatsPerBar * this.clock.beatDur;
		}, {

			// if the clock and the dur disagree?
			(default_dur != this.clock.beatsPerBar * this.clock.beatDur).if ({
				// do we adjust the beat or the duration?
				// adjust the clock

				this.clock.beatDur = default_dur / this.clock.beatsPerBar;
			})
		});

		syllables = [\Do, \Re, \Mi, \Fa, \So, \La, \Si];
		key = Key(Scale.major).change(chromatic: 3); // C maj

		vars = (solfasire:nil, solfasisol:nil, soldosifa:nil);
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



		//default_dur = Ref(init_dur);

		minspace = Ref(0.1); // unref this
		//recording = Ref(false);
		line = DomifareLine();

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
		"define larelasi".postln;

		DomifareCommand.define(\larelasi, 1, 1, [\var], {|lang, varname|  // declare loop
			var loop, action;
			loop = DomifareLoop(varname, this.dur); // ok, here's our new loop
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




		word = '';
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
		"pausing".postln;
		paused= true;
	}

	resume{
		"going".postln;
		paused = false;
	}



	pr_record {|loop, buffer, dur, pad=0.01, in=0, clock, bufferAction, recordAction|

		var loop_dur, numFrames;
		// if buffer is a number, then it's a bufnum;

		loop_dur = dur ? loop.dur? this.dur.value; // if the passed in dur is nil, use the default
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

			Pbind(
				\instrument, \recorder,
				\in, in,
				\buffer, buffer,
				\is_recording, Prout({
					recording = true;
					onsetFunc = 1; // start geting onsets
					this.changed;
					true.yield; // ok, let's go!
					recording = false;
					loop.offsets = onsets; // grab these
					onsetFunc = nil; // stop adding to it
					this.changed; // tell the GUI
					recordAction.value(loop);
					// don't yield. The nil stops the recording from repeating
				})
			).play(clock);

		}.fork;
	}

	default_dur_{|newdur, theChanger|

		default_dur = newdur;
		this.changed(theChanger);
	}



	server_{|srv|

		server = srv;

		server.waitForBoot({


			thresh = Bus.control(server, 1).set(0.2);
			space = Bus.control(server, 1).set(0.5);
			longspace = Bus.control(server, 1).set(2);

			server.sync;

			this.changed(this, this);

			// PUT THIS IN A GUI
			syn = Synth(\domifare_input,
				[\gate, 1, \in, 0, \thresh, thresh.asMap, \space, space.asMap,
					\longspace, longspace.asMap],server);

			// out with the old (if it exists)
			OSCdef(\domifare_in).free;



			// This needs re-writing to deal with the command class

			OSCdef(\domifare_in, {|msg, time, addr, recvPort|
				var tag, node, id, value, letter, result, err;

				#tag, node, id, value = msg;
				//[tag, id, value].postln;
				case
				{ id ==0 } { /* pitch */
					//"pitch".postln;
					recording.not.if({
						//"not recording".postln;
						paused.not.if({
							//"pitch".postln;
							//letter.postln;
							letter = syllables.wrapAt(key.freqToDegree(value.asInteger));

							((time - lastrecv).abs > minspace.value).if({
								// ignore things that follow too close on
								word = (word ++ letter).postln;
								//line.append(letter).changed(\append);
								// alert listeners
								this.code_updated(letter);
								//word.postln;
								lastrecv = time;
							});
						});
					});
				}
				{ id ==1 } { /* onset */

					onsetFunc.notNil.if({
						onsetFunc.value(time);
						onsets = onsets.add(time);
					}, {
						onsets = [];
					});

				}
				{ id ==2 } { /* space */
					recording.not.if({
						paused.not.if({
							active.value.isNil.if({
								// we are on a new command
								active = DomifareCommand(word);
								active.value.isNil.if({
									active = vars[word];
									// do recorder function immediately
									// pause this OSCdef
								});
								active.value.isNil.if({
									// ERROR
									err = NotFound("Command or variable not found: %.".format(word));
									this.error_handler(err);
								} , {
									//line = [active];
								});
							} , {
								// we're on an active command
								// try to match the word to known data or pass the symbol if we can't
								data = numbers[word];
								data.isNil.if({
									data = vars[word];
									data.isNil.if({
										data = word;
								})});
								result = active.var_(data);
								// Error = bad data
								result.isKindOf(Error).if({
									//result.errorString.postln;
									this.error_handler(result);
									result = true;
								});
								// true = command is done
								result.if({ // command is finished
									active = nil
								});
							});
							word = '';
							// altert listeners
							this.code_updated(" ");
							//line.append(" ").changed(\append);
						});
					});
				}
				{ id ==3 } { /* EOL */
					recording.not.if({
						paused.not.if({
							active.notNil.if({
								result = active.eval;
								result.isKindOf(Error).if({
									//result.errorString.postln;
									this.error_handler(result);
								});
								active = nil;
							});
							word = '';
							//line.eol.changed(\eol);
						});
					});
				}
			}, '/tr', server.addr);
		});
	}

	gui {|parent, bounds|
		var gui;
		//guiClass = this.guiClass;
		gui = guiClass.new( this );
		gui.gui( parent, bounds );
		//ui = super.gui(this);
		this.changed(this, this);
		line.addDependant(gui);
		^gui;
	}

	registerCodeListener {|listener|
		codeListeners.isNil.if({
			codeListeners = [listener];
		}, {
			codeListeners.indexOf(listener).isNil.if({
				codeListeners = codeListeners ++ listener;
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
		text.post;
		codeListeners.do({|listener|
			listener.code_update(text);
		})
	}

	code_executed {|text|
		text.postln;
		codeListeners.do({|listener|
			listener.code_execute(text);
		})
	}

	error_handler{|err|
		codeListeners.do({|listener|
			listener.code_error(err);
		});
		//err.throw;
		"".postln;
		err.postln;
	}



}

DomifareError : Error {}

NotFound : DomifareError {}

NoSuchVariable : DomifareError {}


DomifareGui : ObjectGui {

	var history, activeLine, sliders, rec_button, run_button, textView, lineView, font;

	*new {|model|
		^super.new(model).init;
	}

	init {
		model.registerCodeListener(this);
	}

	guiBody { arg view;

		var index;

		//super.guiBody(view);

		"in guiBody".postln;

		sliders = IdentityDictionary.new();

		view.class.postln;
		//view.bounds = Rect(0,0, 1000, 1000);

		run_button = Button(view, 100@20).states_([
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

		rec_button = Button(view, 40@20).states_([
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

			"updating".postln;

			theModel.isKindOf(DomifareLine).if({

				theModel.postln;
				theChanger.postln;

			}, { // else normal model

				// are we recording?
				model.recording.value.if({
					rec_button.value = 1;
				},{
					rec_button.value=0;
				});

				// just grab the value
				sliders[\minspace].value = model.minspace.value;

				// get the busses
				model.thresh.get({|value| "thresh %".format(value).postln;
					AppClock.sched(0.0,{sliders[\thresh].value = value.value.ampdb})});
				model.space.get({|value|
					AppClock.sched(0.0,{sliders[\space].value = value.value})});
				model.longspace.get({|value|
					AppClock.sched(0.0,{sliders[\longspace].value = value.value})});

			})
		});
	}

	code_update{|text|

		AppClock.sched(0.0,{lineView.string = lineView.string ++ text;});
	}

	code_error{|err|
		var text, start, size;

		text = /*"ERROR:" + */ err.errorString;
		size = text.size;

		AppClock.sched(0.0,{
			start = textView.string.size;
			textView.string = textView.string ++ text ++ "\n";
			// fucking stupid shit to fix the scrolling issue
			textView.select(textView.string.size+1, 0);
			textView.stringColor_(Color.white);
			textView.setFont(font);
			//textView.syntaxColorize;
			textView.setStringColor(Color.red, start, size);

			lineView.value = "";
		});

		//err.throw();
		//"".postln;
		//err.postln;
	}

	code_execute{|text|

		AppClock.sched(0.0,{
			lineView.value = "";
			textView.string = textView.string ++ text ++ "\n";
			textView.select(textView.string.size+1, 0);
			textView.stringColor_(Color.white);
			textView.setFont(font);
			//textView.syntaxColorize;
		});
	}

}

DomifareLine {
	var <text;

	*new{|text|
		^super.new.init(text);
	}

	init{|str|
		"init Line".postln;
		text = str;
	}

	append{|str, theChanger|
		text.value_(text.value ++ str);
		this.changed(\append, theChanger);
	}

	clear {
		text = "";
	}

	eol {|theChanger|
		text.changed(\eol, theChanger);
		this.clear;
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
			newindex = vars.size;
			type = types[newindex];
			// check the types
			case
			{ (type == \var) || (type == DomifareLoop) } {
				newvar.isKindOf(DomifareLoop).not.if({
					ret = Error("Variable is wrong type.");
				}, {
					vars = vars ++ newvar;
				});
			}
			{ ( type == \number) || (type == SimpleNumber) } {
				newvar.isKindOf(SimpleNumber).not.if({
					ret = Error("Variable is wrong type.");
				}, {
					vars = vars ++ newvar;
				});
			}
			{ ( type == \operator) || (type = DomifareCommand) } {
				(newvar.isKindOf(Symbol) || newvar.isKindOf(String)).if({
					newvar = DomifareCommand(newvar.asSymbol);
				});
				newvar.notNil.if({
					newvar.isKindOf(DomifareCommand).not.if({
						ret = Error("command not found.");
					}, {
						subcommand = newvar;
						ret = false;
					});
				}, {
					ret = Error("command not found.");
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
		^super.new.copyArgs(name.asSymbol, dur, buffer).offsets_(offsets);
	}

	pr_p {
		var pdef;
		pdef = Pdef(name);
		pdef.source.isNil.if({
			Pdef(name,
				Pbind(
					\instrument, \samplePlayer,
					\bufnum, buffer,
					\db, -3,
					\start_durs, [0, dur],
					\pan, 0.5.rrand(-0.5),
					\rate, 1,
					[\startFrame, \dur], Pfunc({|evt|
						evt[\start_durs]
					})
				)
			)
		})
	}


	pbindef {
		^Pbindef(name);
	}

	pdef {
		^Pdef(name);
	}

	buffer_ { |buf|
		var pd;
		pd = this.pbindef();
		pd.notNil.if({
			Pbindef(name,
				\bufnum, buffer
			);
		});
	}

	offsets_{|offs|

		// passing in an array of offet times can be used to calculate durations
		var diffs, last;

		diffs = offs.differentiate; // diffs[1..n] has the first n-1 durs
		diffs.removeAt(0); // the first item is unchanged by differentiate
		durs = diffs ++ (dur - diffs.sum); // calculate the final duration
		offsets = offs - offs.first; // when does each event start?

		// now pair them
		start_durs = offsets.collect({|item, index|
			[item, durs[index]]
		});
	}



	play {|clock, quant ...args|

		var pd;

		pd = this.pbindef();

		pd.notNil.if({

			clock.isKindOf(ExternalClock).if({ // this also checks for nil
				isplay.isNil.if({ isplay = false });
				isplay.not.if({
					"external".postln;
					pd.playExt(clock, nil, quant);
					isplay = true;
				});
			} , {
				"tempo".postln;
				pd.isPlaying.not.if({
					pd.play(clock, nil, quant);
				});
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
/*

This was based on AlgoRLib, which I wrote no documentation for, so let's put it aside for now
DomifareLoop {

var <name, parent, <pbind, pdef, <>downbeat= -9, <>upbeat= -12, <>offbeat = -13, <>rest= \rest, isplay,
<>beatfigures, <>restfigures, <>playf, <>stopf, loops, index;

*new {|name,parent, pbind|
^super.new.init(name, parent, pbind)
}

init{|na, pa, pb|
"init loop".postln;
name = na;
parent = pa;
pbind = pb;
loops = [];
index = -1;
}


pdef {
pdef.isNil.if({
pdef = Pdef(name);
});
^pdef
}


next {

index = (index + 1).min(loops.size -1);
^loops[index];

}

prev {

index = (index-1).max((loops.size > 0).if({0}, {-1}));
^loops[index];

}

last{
index = loops.size-1;
^loops[index];
}

rand {

index = loops.size.rand;
^loops[index];

}


loop {
^loops[index];
}


play {|clock, quant ...args|

var pd;

playf.notNil.if({
playf.value(*args)
});

pd = this.pdef();

pd.notNil.if({

clock.isKindOf(ExternalClock).if({ // this also checks for nil
isplay.isNil.if({ isplay = false });
isplay.not.if({
"external".postln;
pd.playExt(clock, nil, quant);
isplay = true;
});
} , {
"tempo".postln;
pd.isPlaying.not.if({
pd.play(clock, nil, quant);
});
});
})

}

stop {|...args|
var pd;

stopf.notNil.if({
stopf.value(*args)
});

pd = this.pdef();
pd.notNil.if({pd.stop();});
isplay.notNil.if({ isplay = false});
}


pause {
var pd;

pd = this.pdef();
pd.notNil.if({pd.pause();})

}



isPlaying {

var pd, ret;

pd = this.pdef; //pdef.notNil.if ({ pdef }, { Pdef(name) });
pd.isNil.if ({ ret = false; },
{
ret = isplay;
ret.isNil.if ({
ret = pd.isPlaying;
});
});
ret.isNil.if ({ ret = false; });

^ret;

}

volume_ {|func|
downbeat= func.value(downbeat);
upbeat=func.value(upbeat);
offbeat=func.value(offbeat);
}

vol_{|f| this.volume_(f) }


}
*/

