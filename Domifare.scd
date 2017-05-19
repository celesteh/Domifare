Domifare {

	var syllables, key, responder, vars, numbers, lang, line, word, lastrecv, active, args;

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

	init {|server|

		syllables = [\Do, \Re, \Mi, \Fa, \So, \La, \Si];
		key = Key(Scale.major).change(chromatic: 3); // C maj

		vars = (solfasire:nil, solfasisol:nil, soldosifa:nil);
		numbers = (redodo: 1, remimi:2, refafa: 3, resolsol: 4, relala: 5, resisi: 6, mimido: 7, mimire:8);


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

		line = [];
		args = [];
		word = '';
		lastrecv = 0;

		server.noNil.if({
			this.server_(server);
		});

	}


	server_{|server|

		// out with the old (if it exists)
		OSCdef(\domifare_in).free;



		// This needs re-writing to deal with the command class

		OSCdef(\domifare_in, {|msg, time, addr, recvPort|
			var tag, node, id, value, letter;

			#tag, node, id, value = msg;
			case
			{ id ==0 } { /* pitch */

				letter = syllables.wrapAt(key.freqToDegree(value.asInt));
				// HARDCODED NUMBER ALERT
				((time - lastrecv).abs > 0.1).if({
					// ignore things that follow too close on
					word = word ++ letter;
					lastrecv = time;
				});
			}
			{ id ==1 } { /* onset */ }
			{ id ==3 } { /* space */
				active.isNil.if({
					// we are on a new command
					active = lang[word];
					active.isNil.if({
						active = vars[word];
						// do recorder function immediately
						// pause this OSCdef
					});
					active.isNil.if({
						// ERROR
					} , {
						line = [active];
					});
				} , {
					// we're on an active command
					args = args ++ word;
					// there has to be some way to handle the nesting of chance / loop commmands
					// I need to think about how to do it.
				});
			}
			{ id ==4 } { /* EOL */ }
		}, '/tr', server.addr);

	}


}


DomifareCommand {

	classvar dict;

	*initClass {
		dict = IdentityDictionary.new
	}

	var <name, <minargs, <maxargs, types, func, pause, vars, subcommand;


	*new{|name, minargs, maxargs, types, func, pause|
		var command;
		command = dict[name];
		command.isNil.if({
			minargs.notNil.if({ // don't create a new one for nil values
				command = super.newCopyArgs(name, minargs, maxargs, types, func, pause);
				this.dict.add(name.asSymbol, commmand);
			});
		});
		^command;
	}


	var_{|newvar|
		var newindex, type, ret;
		vars.isNil.if({
			vars = [];
		});

		subcommand.notNil.if({
			ret = subcommand.var_(newvar);
		} , {
			newindex = vars.size;
			type = types[index];
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
				newvar.noNil.if({
					newvar.isKindOf(DomifareCommand).not.if({
						ret = Error("command not found.");
					}, {
						subcommand = newvar;
						ret = false;
					});
				}, {
					ret = Error("command not found.");
				});
			}
			ret.isNil.if({
				(vars.size == maxargs).if ({
					// evaluate immediately
					ret = this.eval
				} , {
					ret = false;
				});
			});
		});

		^ret;

	}

	eval {
		// should return a function to support sub commands

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
}


