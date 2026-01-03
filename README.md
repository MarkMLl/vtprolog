# vtprolog
An archive of Bill and Bev Thompson's "Very Tiny Prolog" in Pascal.

Status:

  v1.1	as released, TP3 (PC-DOS) only.

  v1.2	adds command line handling and a small amount of debug code,
	should be compatible with both TP3 and TP5.5.

Wishlist:

(* Embed halt/0 and halt/1, preferably executed after any user-defined  *)
(* halt/n. Rewrite the hard-coded exit/0 as halt/0 before parsing,	*)
(* although it would be better to reimplement it in the same way as     *)
(* halt/n.                                                              *)
(*                                                                      *)
(* Embed version/0 which prints the version list, version/1 with	*)
(* constant parameter which appends to the version list and version/1   *)
(* with variable parameter which returns the version list. These should *)
(* preferably be executed before any user-defined version/n.            *)
(*                                                                      *)
(* Consider embedded debug/n etc., see http://www.cs.cmu.edu/afs/cs	*)
(* /project/ai-repository/ai/lang/prolog/doc/intro/prolog.doc for good  *)
(* documentation of an early implementation.                            *)
(*                                                                      *)
(* Port to Delphi and to x86 FPC/Lazarus, not necessarily in that	*)
(* order. Note that this will probably break TP3 compatibility, and	*)
(* hopelessly dash the ambitions of anybody expecting it to run on	*)
(* CP/M-80 or CCP/M-86 v3 (it's a cruel world).				*)
(*                                                                      *)
(* Check on SPARC (note: endianness differs which is always a good	*)
(* test of program correctness).                                        *)
(*                                                                      *)
(* Reimplement as an object. Provide a method to add clauses using the  *)
(* implementation of halt/n and version/n (see above) as a model, and   *)
(* replace console output with a callback.                              *)
(*                                                                      *)
(* The result should be something which is functionally close to the    *)
(* original implementation, but can also be embedded in a larger-scale	*)
(* program as a simple inference engine- although I'm damned if I can   *)
(* think of a problem that this particular solution is looking for :-)  *)
