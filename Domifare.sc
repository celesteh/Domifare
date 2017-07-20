Domifare {

	var syllables, key, responder, <vars, <numbers, word, lastrecv, active, data, >onsetFunc, <>onsets,
	paused, syn, <>clock;

	*initClass{

		StartUp.add {
			SynthDef(\domifare_input, { arg gate=0, in=0, thresh=0.2, space=0.5, longspace=2;

				var input, env, fft_pitch, onset, chain, hasfreq, paused, linebreak;

				input = SoundIn.ar(in, 1);
				env = EnvGen.kr(Env.asr, gate, doneAction:2);

				chain = FFT(LocalBuf(2048), input);
				onset = Onsets.kr(chain, odftype:\phase);//odftype:\wphase);
				#fft_pitch, hasfreq = Pitch.kr(input);
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

		}
	}

	init {|server, clock|

		this.clock = clock;

		syllables = [\Do, \Re, \Mi, \Fa, \So, \La, \Si];
		key = Key(Scale.major).change(chromatic: 3); // C maj

		vars = (solfasire:nil, solfasisol:nil, soldosifa:nil);
		numbers = (redodo: 1, remimi:2, refafa: 3, resolsol: 4, relala: 5, resisi: 6, mimido: 7, mimire:8);

		paused = false;

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

		DomifareCommand(\larelasi, 1, 1, [\var], {|lang, varname|
			// add a variable
			//lang.pause; // stop accepting notes as data
			lang.vars.put(varname.asSymbol, nil); // new DomiFareLoop
			//lang.resume; // resume accepting notes as data <-- should go in a done action
		}, this);
		DomifareCommand(\dolamido, 0, 1, [\var], {|lang, loop|
			// stop loop
			lang.stop(loop);
		}, this);
		DomifareCommand(\domilado, 0, 1, [\var], {|lang, loop|
			// start loop
			lang.start(loop);
		}, this);
		DomifareCommand(\lamidore, 1, 1, [\var], {|lang, loop|
			// add notes to loop
			//lang.pause;
			//loop.addNotes;
			//lang.resume;
		}, this);
		DomifareCommand(\misisifa, 0, 1, [\var], {|lang, loop|
			lang.next(loop);
		}, this);
		DomifareCommand(\fasisimi, 0, 1, [\var], {|lang, loop|
			lang.prev(loop);
		}, this);
		DomifareCommand(\misoldola, 0, 1, [\var], {|lang, loop|
			lang.rand(loop);
		}, this);
		//DomifareCommand(\refamido, 0, 0, [], {}); // Just stop all loops instead?
		DomifareCommand(\sifala, 3, 3, [\number, \number, \operator], {|lang, x, y, cmd|
			// this is complicated
		}, this); // X times every Y bars
		DomifareCommand(\larefami, 2, 2, [\number, \operator], {|lang, x, cmd|
			var guess;
			guess = 8.rand + 1;
			(guess < x ).if({ // double check this logic
				cmd.eval.value
			});
		}, this);




		word = '';
		lastrecv = 0;

		server.isNil.if({
			server = Server.default;
		});
		this.server_(server);


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
		paused= true;
	}

	resume{
		paused = false;
	}


	server_{|server|

		server.waitForBoot({
			// PUT THIS IN A GUI
			syn = Synth(\domifare_input, [\gate, 1, \in, 0, \thresh, 0.2, \space, 0.5, \longspace, 2],server);
			// out with the old (if it exists)
			OSCdef(\domifare_in).free;



			// This needs re-writing to deal with the command class

			OSCdef(\domifare_in, {|msg, time, addr, recvPort|
				var tag, node, id, value, letter, result;

				#tag, node, id, value = msg;
				case
				{ id ==0 } { /* pitch */
					paused.not.if({
						letter = syllables.wrapAt(key.freqToDegree(value.asInt));
						// HARDCODED NUMBER ALERT
						((time - lastrecv).abs > 0.1).if({
							// ignore things that follow too close on
							word = (word ++ letter).postln;
							lastrecv = time;
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
					paused.not.if({
						active.isNil.if({
							// we are on a new command
							active = DomifareCommand(word);
							active.isNil.if({
								active = vars[word];
								// do recorder function immediately
								// pause this OSCdef
							});
							active.isNil.if({
								// ERROR
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
								result.errorString.postln;
								result = true;
							});
							// true = command is done
							result.if({ // command is finished
								active = nil
							});
						});
						word = '';
					});
				}
				{ id ==3 } { /* EOL */
					paused.not.if({
						active.notNil.if({
							result = active.eval;
							result.isKindOf(Error).if({
								result.errorString.postln;
							});
							active = nil;
						});
						word = '';
					});

				}
			}, '/tr', server.addr);
		});
	}


}


DomifareCommand {

	classvar dict;
	var <name, <minargs, <maxargs, types, >func, launcher, vars, subcommand;

	*initClass {
		dict = IdentityDictionary.new
	}



	*new{|name, minargs, maxargs, types, func, launcher|
		var command;
		command = dict[name];
		command.isNil.if({
			minargs.notNil.if({ // don't create a new one for nil values
				command = super.newCopyArgs(name, minargs, maxargs, types, func, launcher);
				dict.put(name.asSymbol, command);
			});
		});
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

DomifareLoop {

	var <name, parent, <pbind, pdef, <>downbeat= -9, <>upbeat= -12, <>offbeat = -13, <>rest= \rest, isplay,
	<>beatfigures, <>restfigures, <>playf, <>stopf, loops, index;

	*new {|name,parent, pbind|
		^super.new.init(name, parent, pbind)
	}

	init{|na, pa pb|
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

			clock.isKindOf(ExternalClock).if({
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
		pd.notNil.if({pd.stop();})
	}


	pause {
		var pd;

		pd = this.pdef();
		pd.notNil.if({pd.pause();})

	}



	isPlaying {

		var pd, ret;

		pd = pdef.notNil.if ({ pdef }, { Pdef(name) });
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


