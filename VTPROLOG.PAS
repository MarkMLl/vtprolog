{.PW132}
{.IN+}
{.HE VTPROLOG.PAS                                 Page #}
{$V-,R+,B- }
PROGRAM very_tiny_prolog ;

(* Copyright 1986 - MicroExpert Systems
                    Box 430 R.D. 2
                    Nassau, NY 12123       *)

(* Revisions - 1.1  Nov. 1986   - Edinburgh list syntax added *)

(* I've corrected one minor syntax error without inline comment,	*)
(* otherwise modifications as below: none of these change the original	*)
(* algorithms significantly. The copyright of all modifications in	*)
(* vtprolog 1.x is surrendered to the original authors.			*)
(*                                                                      *)
(* Added USES to allow it to compile with Turbo Pascal v5.5, commenting	*)
(* this out should revert to v3 compatibility subject to source file	*)
(* size etc. Fix code in is_console() which was version-specific. Allow	*)
(* input files to be specified on the command line, .pro assumed in all	*)
(* cases.								*)
(*									*)
(* A command-line option "no-wait", (preceded by /, - or --, case	*)
(* insensitive) will cause all query results to be returned without	*)
(* waiting for keypresses; make a WriteLn conditional so that there is	*)
(* not a gap between each line of results. Also add "echo" and "debug"	*)
(* options, make sure each of these also has a no- form, and finally	*)
(* "no-console" to get output redirection working properly. Add "help"	*)
(* and "version" options to conform with current usage, and hidden	*)
(* "debug2" and "debug3" which increase the amount of debug output.	*)
(*									*)
(* Bump version to 1.2 after consultation with Bill.			*)
(*				MarkMLl @ KDG Telemetry December 2009	*)

(* VTPROLOG implements the data base searching and pattern matching of
   PROLOG. It is described in "PROLOG from the Bottom Up" in issues
   1 and 2 of AI Expert.

   This program has been tested using Turbo ver 3.01A on an IBM PC. It has
   been run under both DOS 2.1 and Concurrent 4.1 .

   We would be pleased to hear your comments, good or bad, or any applications
   and modifications of the program. Contact us at:

     AI Expert
     Miller Freeman Publications
     500 Howard St.
     San Francisco, CA 94105

   Bill and Bev Thompson    *)

USES Turbo3, Crt; (* Comment out manually for Turbo Pascal v3. MarkMLl.	*)

 CONST
  back_space = ^H ;
  tab = ^I ;
  eof_mark = ^Z ;
  esc = #27 ;
  quote_char = #39 ;
  left_arrow = #75 ;
  end_key = #79 ;
  del_line = ^X ;
  return = ^M ;
  bell = ^G ;

  dConstant= '›';	(* Decorations for -debug2, PC-specific. MarkMLl. *)
  dFunctor= 'Ÿ';
  dVariable= '';
  dHead= '';
  dList= 'ð';
  dNil= 'ì';

 TYPE
  counter = 0 .. maxint ;
  string80 = string[80] ;
  string132 = string[132] ;
  string255 = string[255] ;
  text_file = text ;
  char_set = SET OF char ;
  tokenType= (ttHelp, ttVersion, ttNoConsole, ttWait, ttNoWait,
		ttEcho, ttNoEcho, ttDebug, ttNoDebug, ttDebug2,
					ttDebug3, ttOther);	(* MarkMLl *)
  node_type = (cons_node,func,variable,constant,free_node) ;
  node_ptr = ^node ;
  node = RECORD
          in_use : boolean ;
          CASE tag : node_type OF
           cons_node : (tail_ptr : node_ptr ;
                        head_ptr : node_ptr) ;
           func,
           constant,
           variable  : (string_data : string80) ;
           free_node : (next_free : node_ptr ;
                        block_cnt : counter) ;
          END ;

(* node is the basic allocation unit for lists. The fields are used as
   follows:

    in_use     - in_use = false tells the garbage collector that this node
                 is available for re-use.
    tag        - which kind of node this is.
    cons_node  - cons_nodes consist of two pointers,
                 one to the head (first item)
                 the other to the rest of the list. They are the "glue" which
                 holds the list together. The list (A B C) would be stored as
                   -------         --------          --------
                   | .| . |----->  |  .| . |------> |  .| . |---> NIL
                   --|-----         --|------        --|-----
                     |                |                |
                     V                V                V
                     A                B                C

                 The boxes are the cons nodes, the first part of the box
                 holds the head pointer, then second contains the tail.
    constant   - holds string values, we don't actually use the entire 80
                 characters in most cases.
    variable   - also conatins a string value, these nodes will be treated as
                 PROLOG variables rather than constants.
    free_node  - the garbage collector gathers all unused nodes and puts
                 them on a free list. It also compacts the free space into
                 contiguous blocks. next_free points to the next free block.
                 block_cnt contains a count of the number of contiguous 8 byte
                 free blocks which follow this one.    *)


 VAR
  line,saved_line : string132 ;
  token : string80 ;
  source_file : text_file ;
  error_flag,in_comment : boolean;
  no_console, no_wait, do_echo, do_debug, do_debug2, do_debug3: BOOLEAN; (* MarkMLl *)
  delim_set,text_chars : char_set ;
  data_base,initial_heap,free,saved_list : node_ptr ;
  total_free : real ;

(* The important globals are:
   source_file  - text file containing PROLOG statements.
   line         - line buffer for reading in the text file
   saved_list   - list of all items that absolutely must be saved if garbage
                  collection occurs. Usually has at least the data_base and
                  the currents query attached to it.
   initial_heap - the value of the heap pointer at the start of the program.
                  used by the garbage collector
   free         - the list of free nodes.
   total_free   - total number of free blocks on the free list.
   data_base    - a pointer to the start of the data base. It points to a
                  node pointing to the first sentence in the data base. Nodes
                  pointing to sentences are linked together to form the data
                  base.
   delim_set    - set of characters which delimit tokens. *)


(* ----------------------------------------------------------------------
        Utility Routines
   ---------------------------------------------------------------------- *)

 PROCEDURE noise ;
  (* Make a noise on the terminal - used for warnings. *)
  BEGIN
   write(bell) ;
  END ; (* noise *)


 FUNCTION open(VAR f : text_file ; f_name : string80) : boolean ;
  (* open a file - returns true if the file exists and was opened properly
     f      - file pointer
     f_name - external name of the file *)
  BEGIN
   assign(f,f_name) ;
   (*$I- *)
   reset(f) ;
   (*$I+ *)
   open := (ioresult = 0) ;
  END ; (* open *)


 FUNCTION is_console(VAR f : text_file) : boolean ;
  (* return true if f is open on the system console
     for details of fibs and fib_ptrs see the Turbo Pascal ver 3.0 reference
     manual chapter 20. This should work under CP/M-86 or 80, but we haven't
     tried it. *)

  (* The structure representing a text file in the runtime library      *)
  (* differs between TP v3.0 and v5.0. Refer to the manuals, p220 and	*)
  (* p120 respectively. MarkMLl.            				*)

  TYPE
   fib = ARRAY [0 .. 75] OF byte ;
  VAR
   fib_ptr : ^fib ;
   dev_type : byte ;
  BEGIN
    fib_ptr := addr(f) ;
    IF SizeOf(text_file) < 256 THEN BEGIN
      dev_type := fib_ptr^[2] AND $07 ;
      is_console := (dev_type = 1) OR (dev_type = 2) (* Console device  *)
    END ELSE
      is_console:= fib_ptr^[48] = 0     (* No name                      *)
  END ; (* is_console *)


 PROCEDURE strip_leading_blanks(VAR s : string80) ;
  BEGIN
   IF length(s) > 0
    THEN
     IF (s[1] = ' ') OR (s[1] = tab)
      THEN
       BEGIN
        delete(s,1,1) ;
        strip_leading_blanks(s) ;
       END ;
  END ; (* strip_leading_blanks *)


 PROCEDURE strip_trailing_blanks(VAR s : string80) ;
  BEGIN
   IF length(s) > 0
    THEN
     IF (s[length(s)] = ' ') OR (s[length(s)] = tab)
      THEN
       BEGIN
        delete(s,length(s),1) ;
        strip_trailing_blanks(s) ;
       END ;
  END ; (* strip_trailing_blanks *)



 FUNCTION toupper(s : string80) : string80 ;
  (* returns s converted to upper case *)
  VAR
   i : byte ;
  BEGIN
   IF length(s) > 0
    THEN
     FOR i := 1 TO length(s) DO
      s[i] := upcase(s[i]) ;
   toupper := s ;
  END ; (* toupper *)


 FUNCTION is_number(s : string80) : boolean ;
  (* checks to see if s contains a legitimate numerical string.
     It ignores leading and trailing blanks *)
  VAR
   num : real ;
   code : integer ;
  BEGIN
   strip_trailing_blanks(s) ;
   strip_leading_blanks(s) ;
   IF s <> ''
    THEN val(s,num,code)
    ELSE code := -1 ;
   is_number := (code = 0) ;
  END ; (* is_number *)


 FUNCTION cardinal(i : integer) : real ;
  VAR
   r : real ;
  BEGIN
   r := i ;
   IF r < 0
    THEN r := r + 65536.0 ;
   cardinal := r ;
  END ; (* cardinal *)


 FUNCTION head(list : node_ptr) : node_ptr ;
  (* returns a pointer to the first item in the list.
     If the list is empty, it returns NIL.  *)
  BEGIN
   IF list = NIL
    THEN head := NIL
    ELSE head := list^.head_ptr ;
  END ; (* head *)


 FUNCTION tail(list : node_ptr) : node_ptr ;
  (* returns a pointer to a list starting at the second item in the list.
     Note - tail( (a b c) ) points to the list (b c), but
            tail( ((a b) c d) ) points to the list (c d) .  *)
  BEGIN
   IF list = NIL
    THEN tail := NIL
   ELSE
    CASE list^.tag OF
     cons_node : tail := list^.tail_ptr ;
     free_node : tail := list^.next_free ;
     ELSE        tail := NIL ;
    END ;
  END ; (* tail *)


 FUNCTION allocation_size(x : integer) : integer ;
  (* Turbo 3.0 allocates memory in 8 byte blocks, this routine calculates the
     actual number of bytes returned for a request of x bytes.  *)
  BEGIN
   allocation_size := (((x - 1) SHR 3) + 1) SHL 3 ;
  END ; (* allocation_size *)


 FUNCTION node_size : counter ;
  (* calculates the base size of a node. Add the rest of the node to this
     to get the actual size of a node *)
  BEGIN
   node_size := 2 * sizeof(node_ptr) + sizeof(boolean) + sizeof(node_type) ;
  END ; (* node_size *)


 FUNCTION normalize(pt : node_ptr) : node_ptr ;
  (* returns a normalized pointer. Pointers are 32 bit addresses. The first
     16 bits contain the segment number and the second 16 bits contain the
     offset within the segment. Normalized pointers have offsets in the range
     $0 to $F (0 .. 15)    *)
  VAR
   pt_seg,pt_ofs : integer ;
  BEGIN
   pt_seg := seg(pt^) + (ofs(pt^) SHR 4) ;
   pt_ofs := ofs(pt^) AND $000F ;
   normalize := ptr(pt_seg,pt_ofs) ;
  END ; (* normalize *)

 FUNCTION hexA(a: node_ptr): string80;

 (* Output a constituent of a node, i.e. an address, as text for	*)
 (* advanced debugging.							*)
 (* conditional compilation.						*)

   FUNCTION hexW(i: INTEGER): string80;

     FUNCTION hexB(b: INTEGER): string80;

       FUNCTION hexN(n: INTEGER): CHAR;

       BEGIN
	 CASE n OF
	   0..
	   9: hexN:= CHR(ORD('0') + n);
	 ELSE
	   hexN:= CHR(ORD('A') + n - 10)
	 END
       END (* hexN *) ;

     BEGIN
       hexB:= hexN(b DIV 16) + hexN(b MOD 16)
     END (* hexB *) ;

   BEGIN
     hexW:= hexB(i DIV 256) + hexB(i MOD 256)
   END (* hexW *) ;

 BEGIN
   hexA:= hexW(Seg(a^)) + ':' + hexW(Ofs(a^))
 END (* hexA *) ;

 FUNCTION string_val(list : node_ptr) : string80 ;
  (* returns the string pointed to by list. If list points to a number
     node, it returns a string representing that number *)
  VAR
   s : string[15] ;
  BEGIN
   IF list = NIL
    THEN
     BEGIN
      IF do_debug2			(* Decorations. MarkMLl.	*)
       THEN string_val := 'dNil'
       ELSE string_val:= '';
      IF do_debug3
       THEN string_val := concat(concat(dNil, hexA(NIL)), ' ')
     END
    ELSE IF list^.tag IN [constant,variable,func]
     THEN string_val := list^.string_data
     ELSE string_val := '' ;
  END ; (* string_val *)


 FUNCTION tag_value(list : node_ptr) : node_type ;
  (* returns the value of the tag for a node.     *)
  BEGIN
   IF list = NIL
    THEN tag_value := free_node
    ELSE tag_value := list^.tag ;
  END ; (* tag_value *)


 PROCEDURE print_list(list : node_ptr) ;
  (* recursively traverses the list and prints its elements. This is
     not a pretty printer, so the lists may look a bit messy.  *)

  VAR
   p : node_ptr ;
  BEGIN
   IF list <> NIL
    THEN
     CASE list^.tag OF
      constant  : BEGIN			(* Decorations. MarkMLl.	*)
		    IF do_debug2 THEN
		      Write(dConstant);
		    IF do_debug3 THEN
		      Write(hexA(list), ' ');
		    write(string_val(list),' ')
		  END;
      func      : BEGIN
		    IF do_debug2 THEN
		      Write(dFunctor);
		    IF do_debug3 THEN
		      Write(hexA(list), ' ');
		    write(string_val(list),' ')
		  END;
      variable  : BEGIN
		    IF do_debug2 THEN
		      Write(dVariable);
		    IF do_debug3 THEN
		      Write(hexA(list), ' ');
		    write(string_val(list),' ')
		  END;
      cons_node : BEGIN
                   write('(') ;
                   p := list ;
                   WHILE p <> NIL DO
                    BEGIN
                     IF tag_value(p) = cons_node
                      THEN BEGIN
			IF do_debug2 THEN
			  Write(dHead);
		    	IF do_debug3 THEN
		      	  Write(hexA(head(p)), ' ');
		        print_list(head(p))
		      END ELSE BEGIN
			IF do_debug2 THEN
			  Write(dList);
		    	IF do_debug3 THEN
		      	  Write(hexA(p), ' ');
		        print_list(p)
		      END;
                     p := tail(p) ;
                    END ;
                   write(') ') ;
                  END ;
     END
   ELSE
    BEGIN
     IF do_debug2 THEN
       Write(dNil);
     IF do_debug3 THEN
       Write(hexA(NIL), ' ')
    END
  END ; (* print_list *)


 PROCEDURE get_memory(VAR pt : node_ptr ; size : integer) ;
  (* On exit p contains a pointer to a block of allocation_size(size) bytes.
     If possible this routine tries to get memory from the free list before
     requesting it from the heap *)
  VAR
   blks : counter ;
   allocated : boolean ;

  PROCEDURE get_from_free ;
   VAR
    p,q : node_ptr ;
   BEGIN
    p := free ;
    q := p ;
    WHILE (p <> NIL) AND (NOT allocated) DO
     IF p^.block_cnt >= (blks - 1)
      THEN
       BEGIN
        pt := normalize(ptr(seg(p^),ofs(p^) + (p^.block_cnt - blks + 1) SHL 3)) ;
        IF p^.block_cnt = (blks - 1)
         THEN
          IF p = free
           THEN free := free^.next_free
           ELSE q^.next_free := p^.next_free
         ELSE p^.block_cnt := p^.block_cnt - blks ;
        allocated := true ;
        total_free := total_free - (blks * 8.0) ;
       END
      ELSE
       BEGIN
        q := p ;
        p := p^.next_free ;
       END ;
   END ; (* get_from_free *)

  BEGIN
   blks := ((size - 1) SHR 3) + 1 ;
   allocated := false ;
   get_from_free ;
   IF NOT allocated
    THEN getmem(pt,blks SHL 3) ;
  END ; (* get_memory *)


 FUNCTION alloc_str(typ : node_type ; s : string80) : node_ptr ;
  (* Allocate storage for a string and return a pointer to the new node.
     This routine only allocates enough storage for the actual number of
     characters in the string plus one for the length. Because of this,
     concatenating anything to the end of a string stored in a symbol node
     will lead to disaster. Copy the string to a new string do the
     concatenation and then allocate a new node.  *)
  VAR
   pt : node_ptr ;
  BEGIN
   get_memory(pt,allocation_size(sizeof(node_type) + sizeof(boolean) +
                                 length(s) + 1)) ;
   pt^.tag := typ   ;
   pt^.string_data := s ;
   alloc_str := pt ;
  END ; (* alloc_str *)


 FUNCTION cons(new_node,list : node_ptr) : node_ptr ;
  (* Construct a list. This routine allocates storage for a new cons node.
     new_node points to the new head of the list. The tail pointer of the
     new node points to list. This routine adds the new cons node to the
     beginning of the list and returns a pointer to it. The list described
     in the comments at the beginning of the program could be constructed
     as cons(alloc_str('A'),cons(alloc_str('B'),cons(alloc_str('C'),NIL))). *)
  VAR
   p : node_ptr ;
  BEGIN
   get_memory(p,allocation_size(node_size)) ;
   p^.tag := cons_node ;
   p^.head_ptr := new_node ;
   p^.tail_ptr := list ;
   cons := p ;
  END ; (* cons *)


 FUNCTION append_list(list1,list2 : node_ptr) : node_ptr ;
  (* Append list2 to list1. This routine returns a pointer to the
     combined list. Appending is done by consing each item on the first
     list to the second list. This routine is one of the major sources of
     garbage so if garbage collection becomes a problem, you may want to
     rewrite it. *)
  BEGIN
   IF list1 = NIL
    THEN append_list := list2
    ELSE append_list := cons(head(list1),append_list(tail(list1),list2)) ;
  END ; (* append_list *)


 FUNCTION list_length(list : node_ptr) : counter ;
  (* returns the length of a list.
     Note - both (A B C) and ( (A B) C D) have length 3.   *)
  BEGIN
   IF list = NIL
    THEN list_length := 0
    ELSE list_length := 1 + list_length(list^.tail_ptr) ;
  END ; (* list_length *)


 PROCEDURE collect_garbage ;
  (* This routine is specific to Turbo Pascal Ver 3.01
     It depends upon the fact that Turbo allocates memory in 8 byte blocks
     on the PC. If you recompile this program on another system be very
     careful with this routine.
     Garbage collection proceeds in three phases:
      unmark  - free all memory between the initial_heap^ and the current
                top of the heap.
      mark    - mark everything on the saved_list as being in ues.
      release - gather all unmarked blocks and put them on the free list.
     The collector displays a '*' on the screen to let you know it is
      operating.  *)

  FUNCTION lower(p1,p2 : node_ptr) : boolean ;
   (* returns true if p1 points to a lower memory address than p2 *)
   BEGIN
    p1 := normalize(p1) ;
    p2 := normalize(p2) ;
    lower := (cardinal(seg(p1^)) < cardinal(seg(p2^))) OR
              ((seg(p1^) = seg(p2^)) AND
              (cardinal(ofs(p1^)) < cardinal(ofs(p2^)))) ;
   END ; (* lower *)

  PROCEDURE mark(list : node_ptr) ;
   (* Mark the blocks on list as being in use. Since a node may be on several
      lists at one time, if it is already marked we don't continue processing
      the tail of the list. *)
   BEGIN
    IF list <> NIL
     THEN
      BEGIN
       IF NOT list^.in_use
        THEN
         BEGIN
          list^.in_use := true ;
          IF list^.tag = cons_node
           THEN
            BEGIN
             mark(head(list)) ;
             mark(tail(list)) ;
            END ;
         END ;
      END ;
   END ; (* mark *)

  PROCEDURE unmark_mem ;
   (* Go through memory from initial_heap^ to HeapPtr^ and mark each node
      as not in use. The tricky part here is updating the pointer p to point
      to the next cell. *)
   VAR
    p : node_ptr ;
    string_base,node_allocation : integer ;
   BEGIN
    string_base := sizeof(node_type) + sizeof(boolean) ;
    p := normalize(initial_heap) ;
    node_allocation := allocation_size(node_size) ;
    WHILE lower(p,HeapPtr) DO
     BEGIN
      p^.in_use := false ;
      CASE p^.tag OF
       cons_node : p := normalize(ptr(seg(p^),ofs(p^) + node_allocation)) ;
       free_node : p := normalize(ptr(seg(p^),ofs(p^) + (p^.block_cnt + 1) * 8)) ;
       func,
       constant,
       variable  : p := normalize(ptr(seg(p^),
                                  ofs(p^) +
                                  allocation_size(string_base +
                                                  length(p^.string_data) + 1))) ;
      END ;
     END ;
   END ; (* unmark_mem *)

  PROCEDURE release_mem ;
   (* This procedure does the actual collection and compaction of nodes.
      This is the slow phase of garbage collection because of all the pointer
      manipulation.  *)
   VAR
    heap_top : node_ptr ;
    string_base,node_allocation,string_allocation,block_allocation : integer ;

   PROCEDURE free_memory(pt : node_ptr ; size : counter) ;
    (* return size bytes pointed to by pt to the free list. If pt points to
       a block next to the head of the free list combine it with the top
       free node. total_free keeps track of the total number of free bytes. *)
    VAR
     blks : counter ;
    BEGIN
     IF pt <> NIL
      THEN
       BEGIN
        blks := ((size - 1) SHR 3) + 1 ;
        pt^.tag := free_node ;
        IF normalize(ptr(seg(pt^),ofs(pt^) + (blks SHL 3))) = free
         THEN
          BEGIN
           pt^.next_free := free^.next_free ;
           pt^.block_cnt := free^.block_cnt + blks ;
           free := pt ;
          END
        ELSE IF normalize(ptr(seg(free^),ofs(free^) +
                      ((free^.block_cnt + 1) SHL 3))) = normalize(pt)
         THEN free^.block_cnt := free^.block_cnt + blks
         ELSE
          BEGIN
           pt^.next_free := free ;
           pt^.block_cnt := blks - 1 ;
           free := pt ;
          END ;
        total_free := total_free + (blks * 8.0) ;
       END ;
    END ; (* free_memory *)

   PROCEDURE do_release ;
    (* This routine sweeps through memory and checks for nodes with
       in_use = false. *)
    VAR
     p : node_ptr ;
    BEGIN
     p := normalize(initial_heap) ;
     WHILE lower(p,heap_top) DO
      CASE p^.tag OF
       cons_node : BEGIN
                    IF NOT p^.in_use
                     THEN free_memory(p,node_size) ;
                    p := normalize(ptr(seg(p^),ofs(p^) + node_allocation)) ;
                   END ;
       free_node : BEGIN
                    block_allocation := (p^.block_cnt + 1) * 8 ;
                    free_memory(p,block_allocation) ;
                    p := normalize(ptr(seg(p^),ofs(p^) + block_allocation)) ;
                   END ;
       func,
       constant,
       variable  : BEGIN
                    string_allocation := allocation_size(string_base +
                                                length(p^.string_data) + 1) ;
                    IF NOT p^.in_use
                     THEN free_memory(p,string_base + length(p^.string_data)
                                      + 1) ;
                    p := normalize(ptr(seg(p^),ofs(p^) + string_allocation)) ;
                   END ;
      END ;
    END ; (* do_release *)

   BEGIN
    free := NIL ;
    total_free := 0.0 ;
    heap_top := HeapPtr ;
    string_base := sizeof(node_type) + sizeof(boolean) ;
    node_allocation := allocation_size(node_size) ;
    do_release ;
   END ; (* release_mem *)

  BEGIN
   write('*') ;
   unmark_mem ;
   mark(saved_list) ;
   release_mem ;
   write(back_space) ;
   IF NOT no_console THEN		(* MarkMLl *)
     clreol ;
  END ; (* collect_garbage *)


 PROCEDURE test_memory ;
  (* This routine activates the garbage collector, if the the total available
     memory (free_list + heap) is less than a specified amount. Lowering the
     minimum causes garbage collection to be called less often, but if you
     make it too small you may not have enough room left for recursion or any
     temporary lists you need. Using 10000 is probably being overly
     cautious.   *)
  BEGIN
   IF (memavail * 16.0) + total_free < 10000
    THEN collect_garbage ;
  END ; (* test_memory *)


 PROCEDURE wait ;
  (* Just like it says. It waits for the user to press a key before
     continuing. *)
  VAR
   ch : char ;
  BEGIN
   writeln ;
   writeln ;
   write('Press any key to continue. ') ;
   read(kbd,ch) ;
   write(return) ;
   IF NOT no_console THEN		(* MarkMLl *)
     clreol ;
  END ; (* wait *)


(* ------------------------------------------------------------------------
        End of utility routines
   ------------------------------------------------------------------------ *)

 PROCEDURE read_kbd(VAR s : string80) ;
  (* Read a line from the keyboard *)
  BEGIN
   IF do_debug THEN BEGIN		(* MarkMLl *)
     Write('%% Free heap: ');
     IF MemAvail >= 0 THEN
       Write(MemAvail)
     ELSE
       Write((65536.0 + MemAvail):1:0);
     WriteLn(' (', total_free:1:0, ')')
   END;
   write('-> ') ;
   readln(s) ;
  END ; (* read_kbd *)


 PROCEDURE read_from_file(VAR f : text_file) ;
  (* Read a line from file f and store it in the global variable line.
     It ignores blank lines and when the end of file is reached an
     eof_mark is returned. *)

  PROCEDURE read_a_line ;
   BEGIN
    (*$I- *)
    readln(f,line) ;
    (*$I+ *)
    IF ioresult <> 0
     THEN line := eof_mark
    ELSE IF eof(f)
     THEN line := concat(line,eof_mark) ;
    IF do_echo AND NOT is_console(f) THEN	(* MarkMLl *)
      IF (IORESULT = 0) AND NOT EOF(f) THEN
        WriteLn(line)
      ELSE
	WriteLn
   END ; (* read_a_line *)

  BEGIN
   line := '' ;
   IF is_console(f)
    THEN read_kbd(line)
    ELSE read_a_line ;
   IF in_comment
    THEN
     IF pos('*)',line) > 0
      THEN
       BEGIN
        delete(line,1,pos('*)',line) + 1) ;
        in_comment := false ;
       END
      ELSE read_from_file(f) ;
   saved_line := line ;
  END ; (* read_from_file *)


 PROCEDURE get_token(VAR t_line : string132 ; VAR token : string80) ;
  (* Extract a token from t_line. Comments are ignored. A token is
     a string surrounded by delimiters or an end of line. Tokens may
     contain embedded spaces if they are surrounded by quote marks *)

  PROCEDURE get_word ;
   VAR
    done : boolean ;
    cn : integer ;
    len : byte ;
   BEGIN
    cn := 1 ;
    len := length(t_line) ;
    done := false ;
    WHILE NOT done DO
     IF cn > len
      THEN done := true
     ELSE IF t_line[cn] IN delim_set
      THEN done := true
     ELSE cn := cn + 1 ;
    token := copy(t_line,1,cn-1) ;
    delete(t_line,1,cn-1) ;
   END ; (* get_word *)

  PROCEDURE comment ;
   BEGIN
    IF pos('*)',t_line) > 0
     THEN
      BEGIN
       delete(t_line,1,pos('*)',t_line)+1) ;
       get_token(line,token) ;
      END
     ELSE
      BEGIN
       t_line := '' ;
       token := '' ;
       in_comment := true ;
      END ;
   END ; (* comment *)

  PROCEDURE get_quote ;
   BEGIN
    delete(t_line,1,1) ;
    IF pos(quote_char,t_line) > 0
     THEN
      BEGIN
       token := concat(quote_char,copy(t_line,1,pos(quote_char,t_line) - 1)) ;
       delete(t_line,1,pos(quote_char,t_line)) ;
      END
     ELSE
      BEGIN
       token := t_line ;
       t_line := '' ;
      END ;
   END ; (* get_quote *)

  BEGIN
   strip_leading_blanks(t_line) ;
   IF length(t_line) > 0
    THEN
     BEGIN
      IF copy(t_line,1,2) = '(*'
       THEN comment
      ELSE IF (copy(t_line,1,2) = ':-') OR (copy(t_line,1,2) = '?-')
       THEN
        BEGIN
         token := copy(t_line,1,2) ;
         delete(t_line,1,2) ;
        END
      ELSE IF t_line[1] = quote_char
       THEN get_quote
      ELSE IF t_line[1] IN delim_set
       THEN
        BEGIN
         token := t_line[1] ;
         delete(t_line,1,1) ;
        END
      ELSE get_word ;
     END
    ELSE token := '' ;
  END ; (* get_token *)


 PROCEDURE scan(VAR f : text_file ; VAR token : string80) ;
  (* Scan repeatedly calls get_token to retreive tokens. When the
     end of a line has been reached, read_from_file is called to
     get a new line. *)
  BEGIN
   IF length(line) > 0
    THEN
     BEGIN
      get_token(line,token) ;
      IF token = ''
       THEN scan(f,token) ;
     END
    ELSE
     BEGIN
      read_from_file(f) ;
      scan(f,token) ;
     END ;
  END ; (* scan *)

  FUNCTION classifyToken(token: string80): tokenType;

  (* Look at the token and decide whether it is a command-line option.	*)
  (* This will only be called when processing the command line, so	*)
  (* doesn't know about general Prolog syntax. I've written this to	*)
  (* handle -, -- and / as option indicators, I'm also allowing /- and	*)
  (* anybody who calls that a bug can fix it, note the precautions	*)
  (* preventing -no-wait being recognised as --wait etc. MarkMLl.	*)

  VAR	scratch: string80;

  BEGIN
    classifyToken:= ttOther;
    IF Length(token) < 3 THEN
      EXIT;
    IF (token[1] = '-') OR (token[1] = '/') THEN BEGIN
      scratch:= toUpper(token);
      Delete(scratch, 1, 1);
      IF (scratch = 'HELP') OR (scratch = '-HELP') THEN
        classifyToken:= ttHelp;
      IF (scratch = 'VERSION') OR (scratch = '-VERSION') THEN
        classifyToken:= ttVersion;
      IF (scratch = 'NO-CONSOLE') OR (scratch = '-NO-CONSOLE') THEN
        classifyToken:= ttNoConsole;
      IF (scratch = 'NO-WAIT') OR (scratch = '-NO-WAIT') THEN BEGIN
	classifyToken:= ttNoWait;
	EXIT
      END;
      IF (scratch = 'WAIT') OR (scratch = '-WAIT') THEN BEGIN
	classifyToken:= ttWait;
	EXIT
      END;
      IF (scratch = 'NO-ECHO') OR (scratch = '-NO-ECHO') THEN BEGIN
	classifyToken:= ttNoEcho;
	EXIT
      END;
      IF (scratch = 'ECHO') OR (scratch = '-ECHO') THEN BEGIN
	classifyToken:= ttEcho;
	EXIT
      END;
      IF (scratch = 'DEBUG2') OR (scratch = '-DEBUG2') THEN BEGIN
	classifyToken:= ttDebug2;
	EXIT
      END;
      IF (scratch = 'DEBUG3') OR (scratch = '-DEBUG3') THEN BEGIN
	classifyToken:= ttDebug3;
	EXIT
      END;
      IF (scratch = 'NO-DEBUG') OR (scratch = '-NO-DEBUG') THEN BEGIN
	classifyToken:= ttNoDebug;
	EXIT
      END;
      IF (scratch = 'DEBUG') OR (scratch = '-DEBUG') THEN BEGIN
	classifyToken:= ttDebug;
	EXIT
      END
    END
  END (* classifyToken *) ;


 PROCEDURE compile(VAR source : text_file) ;
  (* The recursive descent compiler. It reads tokens until the token
     'EXIT' is found. If the token is '?-', a query is performed, a '@' token
     is the command to read a new file and source statements are read form that
     file, otherwise the token is assumed to be part of a sentence and the rest
     of the sentence is parsed. *)

  VAR	paramIndex: INTEGER;		(* MarkMLl *)

  PROCEDURE error(error_msg : string80) ;
   (* Signal an error. Prints saved_line to show where the error is located.
      saved_line contains the current line being parsed. it is required,
      because get_token chews up line as it reads tokens. *)

   PROCEDURE runout ;
    BEGIN
     WHILE (token <> '.') AND (token <> eof_mark) DO
      scan(source,token) ;
    END ; (* runout *)

   BEGIN
    error_flag := true ;
    writeln ;
    writeln(error_msg) ;
    writeln ;
    IF saved_line <> '' THEN BEGIN	(* Blank if file read error. MarkMLl *)
      writeln(saved_line) ;
      writeln('' : length(saved_line) - length(line) - 1,'^') ; ;
    END;
    IF is_console(source)
     THEN
      BEGIN
       token := '.' ;
       line := '' ;
      END
     ELSE runout ;
    IF NOT no_wait THEN			(* MarkMLl *)
      wait ;
   END ; (* error *)

  PROCEDURE goal(VAR l_ptr : node_ptr) ;
   (* Read a goal. The new goal is appended to l_ptr. Each goal is appended
      to l_ptr as a list. Thus, the sentence 'likes(john,X) :- likes(X,wine) .'
      becomes the list ( (likes john X) (likes X wine) ) *)
   VAR
    goal_token : string80 ;

   PROCEDURE functor(VAR f_ptr : node_ptr ; func_token : string80) ;
    (* The current goal is a functor. This routine allocates a node
       to store the functor and then processes the components of the
       functor. On exit, f_ptr points to the list containing the functor
       and its components. func_token contains the functor name. *)
    VAR
     c_ptr : node_ptr ;

    PROCEDURE components(VAR cm_ptr : node_ptr) ;
     (* Process the components of the functor. The components are terms
        seperated by commas. On exit, cm_ptr points to the list of
        components. *)

     PROCEDURE term(VAR t_ptr : node_ptr) ;
      (* Process a single term. The new term is appended to t_ptr. *)
      VAR
       t_token : string80 ;

      PROCEDURE quoted_str(VAR q_ptr : node_ptr) ;
       (* Process a quote *)
       BEGIN
        q_ptr := append_list(q_ptr,cons(alloc_str(constant,
                                        copy(token,2,length(token) - 1)),
                                        NIL)) ;
        scan(source,token) ;
       END ; (* quoted_str *)

      PROCEDURE varbl(VAR v_ptr : node_ptr) ;
       (* The current token is a varaible, allocate a node and return
          a pointer to it. *)
       BEGIN
        v_ptr := append_list(v_ptr,cons(alloc_str(variable,token),NIL)) ;
        scan(source,token) ;
       END ; (* varbl *)

      PROCEDURE number(VAR n_ptr : node_ptr) ;
       (* Numbers are treated as string constants. This isn't particularly
          efficent, but it is easy. *)
       BEGIN
        n_ptr := append_list(n_ptr,cons(alloc_str(constant,token),NIL)) ;
        scan(source,token) ;
       END ; (* number *)

      PROCEDURE list(VAR l_ptr : node_ptr) ;
       (* A list may either be empty, [], or it may be an group of
          elements surrounded by brackets. On return, l_ptr has the
          list structure appended to it. *)
       VAR
        elem_list : node_ptr ;

       PROCEDURE element_list(VAR e_list : node_ptr) ;
        (* The element list is a group of terms separated by commas *)
        VAR
         e_list2 : node_ptr ;
        BEGIN
         term(e_list) ;
         IF token = ','
          THEN
           BEGIN
            scan(source,token) ;
            element_list(e_list) ;
           END
         ELSE IF token = '|'
          THEN
           BEGIN
            e_list2 := NIL ;
            scan(source,token) ;
            term(e_list2) ;
            e_list := append_list(e_list,head(e_list2)) ;
           END ;
        END ; (* element_list *)

       BEGIN
        scan(source,token) ;
        IF token = ']'
         THEN
          BEGIN
           l_ptr := append_list(l_ptr,cons(NIL,NIL)) ;
           scan(source,token) ;
          END
         ELSE
          BEGIN
           elem_list := NIL ;
           element_list(elem_list) ;
           IF token = ']'
            THEN
             BEGIN
              scan(source,token) ;
              l_ptr := append_list(l_ptr,cons(elem_list,NIL)) ;
             END
            ELSE error('Missing '']''.') ;
          END ;
       END ; (* list *)

      BEGIN
       IF token[1] IN ['A' .. 'Z','_']
        THEN varbl(t_ptr)
       ELSE IF token[1] = quote_char
        THEN quoted_str(t_ptr)
       ELSE IF is_number(token)
        THEN number(t_ptr)
       ELSE IF token[1] = '['
        THEN list(t_ptr)
       ELSE IF token[1] IN ['a' .. 'z']
        THEN
         BEGIN
          t_token := token ;
          scan(source,token) ;
          IF token = '('
           THEN functor(t_ptr,t_token)
           ELSE t_ptr := append_list(t_ptr,
                                     cons(alloc_str(constant,t_token),NIL)) ;
         END
       ELSE error('Illegal Symbol.') ;
      END ; (* term *)

     BEGIN
      term(cm_ptr) ;
      IF token = ','
       THEN
        BEGIN
         scan(source,token) ;
         components(cm_ptr) ;
        END ;
     END ; (* components *)

    BEGIN
     c_ptr := cons(alloc_str(func,func_token),NIL) ;
     scan(source,token) ;
     components(c_ptr) ;
     IF token = ')'
      THEN
       BEGIN
        f_ptr := append_list(f_ptr,cons(c_ptr,NIL)) ;
        scan(source,token) ;
       END
      ELSE error('Missing '')''.') ;
    END ; (* functor *)

   BEGIN
    IF token[1] IN ['a' .. 'z',quote_char]
     THEN
      BEGIN
       IF token[1] = quote_char
        THEN
         BEGIN
          l_ptr := append_list(l_ptr,cons(cons(alloc_str(constant,
                               copy(token,2,length(token) - 1)),NIL),NIL)) ;
          scan(source,token) ;
         END
        ELSE
         BEGIN
          goal_token := token ;
          scan(source,token) ;
          IF token = '('
           THEN functor(l_ptr,goal_token)
           ELSE l_ptr := append_list(l_ptr,
                                     cons(cons(alloc_str(constant,goal_token),
                                               NIL),NIL)) ;
         END
      END
     ELSE error('A goal must begin with ''a .. z'' or be a quoted string.') ;
   END ; (* goal *)

  PROCEDURE tail_list(VAR t_ptr : node_ptr) ;
   (* Process the tail of a rule. Since the a query is syntactically identical
      to the tail of a rule, this routine is used to compile queries.
      On exit, t_ptr points to the list containing the tail. *)
   BEGIN
    goal(t_ptr) ;
    IF token = ','
     THEN
      BEGIN
       scan(source,token) ;
       tail_list(t_ptr) ;
      END ;
   END ; (* tail_list *)

  PROCEDURE rule ;
   (* Procees a rule, actually any sentence. If no error occurs the
      new sentence is appended to the data base. *)
   VAR
    r_ptr, new_rule : node_ptr ;

   PROCEDURE head_list(VAR h_ptr : node_ptr) ;
    BEGIN
     goal(h_ptr) ;
    END ; (* head *)

   BEGIN
    IF do_debug THEN			(* MarkMLl *)
      WriteLn('%% In rule(), processing "', token, '"');
    saved_list := cons(data_base,NIL) ;
    test_memory ;
    r_ptr := NIL ;
    head_list(r_ptr) ;
    IF token = ':-'
     THEN
      BEGIN
       scan(source,token) ;
       tail_list(r_ptr) ;
      END ;
    IF token <> '.'
     THEN error('''.'' expected.') ;
    IF NOT error_flag
     THEN
      BEGIN
       new_rule:= cons(r_ptr,NIL);

   (* We have a syntactically-valid rule or fact. Potentially, we could	*)
   (* check here for anything special such as an attempted definition	*)
   (* of exit/0. MarkMLl.						*)

       IF do_debug THEN BEGIN (* MarkMLl *)
         Write('%% new_rule -> ');
         print_list(new_rule);
         WriteLn
       END;
       data_base := append_list(data_base,new_rule) ;
       IF do_debug AND do_debug2 THEN BEGIN (* MarkMLl *)
         Write('%% data_base -> ');
         print_list(data_base);
         WriteLn
       END
      END

   (* In the case where a rule is malformed making an explicit call to	*)
   (* the garbage collector to prune the redundant list is a useful	*)
   (* test case. MarkMLl.						*)

     ELSE
      collect_garbage
   END ; (* rule *)

  PROCEDURE query ;
   (* Process a query. Compile the query, and then call solve to search the
      data base. q_ptr points to the compiled query and solved is a boolean
      indicating whether the query was successfully solved. *)
   VAR
    q_ptr : node_ptr ;
    solved : boolean ;

   PROCEDURE solve(list,env : node_ptr ; level : counter) ;
    (* This is where all the hard work is done. This routine follows the
       steps outlined in the article. list is the query to be soved, env is
       the current environment and level is the recursion level. level can
       only get to 32767, but you'll run out of stack space long before you
       get that far.
       solve saves list and env on the saved list so that they won't be
       destroyed by garbage collection. The data base is always on the
       saved list. At the end of solve, list and env are removed from
       saved_list. *)

   (* Bill confirms that the database state should be the same on exit	*)
   (* from solve() as it was on entry, and that storage used by the	*)
   (* query could potentially be on a private heap (it was originally a	*)
   (* separate list). This suggests a number of interesting lines of	*)
   (* research. MarkMLl.						*)

    VAR
     new_env,p : node_ptr ;

    FUNCTION look_up(var_str : string80 ; environ : node_ptr) : node_ptr ;
     (* Search the environment list pointed to by environ for the variable,
        var_str. If found return a pointer to var_str's binding, otherwise
        return NIL *)
     VAR
      found : boolean ;
      p : node_ptr ;
     BEGIN
      p := environ ;
      found := false ;
      WHILE (p <> NIL) AND (NOT found) DO
       BEGIN
        IF var_str = string_val(head(head(p)))
         THEN
          BEGIN
           found := true ;
           look_up := tail(head(p)) ;
          END
         ELSE p := tail(p) ;
       END ;
      IF NOT found
       THEN look_up := NIL ;
     END ; (* look_up *)

    PROCEDURE check_continue ;
     (* Print the bindings and see if the user is satisfied. If nothing
        is printed from the environment, then print 'Yes' to indicate
        that the query was successfully satisfied. *)
     VAR
      printed,list_printing : boolean ;
      ch : char ;

     PROCEDURE print_bindings(list : node_ptr) ;
      (* Print the bindings for level 0 variables only, intermediate variables
         aren't of interest. The routine recursivley searches for the
         end of the environments list and then prints the binding. This
         is so that variables bound first are printed first. *)

      PROCEDURE print_a_list(l : node_ptr) ; FORWARD ;

      PROCEDURE print_components(p : node_ptr) ; FORWARD ;

      PROCEDURE print_variable(var_str : string80) ;
       (* The varaible in question was bound to another varaible, so look
          up that variable's binding and print it. If a match can't be found
          print '_' to tell the user that the variable is anonymous. *)
       VAR
        var_ptr : node_ptr ;
       BEGIN
        var_ptr := look_up(var_str,env) ;
        IF var_ptr <> NIL
         THEN
          CASE tag_value(var_ptr) OF
           constant  : write(string_val(var_ptr),' ') ;
           variable  : print_variable(string_val(var_ptr)) ;
           cons_node : IF list_printing
                        THEN print_components(var_ptr)
                        ELSE print_a_list(var_ptr) ;
          END
         ELSE write('_ ') ;
       END ; (* print_variable *)

      PROCEDURE print_func(p : node_ptr) ;
       BEGIN
        write(string_val(head(p))) ;
        write('(') ;
        print_components(tail(p)) ;
        write(')') ;
       END ; (* print_func *)

      PROCEDURE print_components (* p : node_ptr *) ;
       (* Print the components of a functor. These may be variables or
          other functors, so call the approriate routines to print them. *)
       BEGIN
        IF p <> NIL
         THEN
          CASE tag_value(p) OF
           constant  : write(string_val(p),' ') ;
           variable  : print_variable(string_val(p)) ;
           cons_node : IF tag_value(head(p)) = func
                        THEN print_func(p)
                        ELSE
                         BEGIN
                          IF tag_value(head(p)) = cons_node
                           THEN print_a_list(head(p))
                           ELSE print_components(head(p)) ;
                          IF tail(p) <> NIL
                           THEN
                            BEGIN
                             write(',') ;
                             print_components(tail(p)) ;
                            END ;
                         END ;
          END ;
       END ; (* print_components *)

      PROCEDURE print_a_list (* l : node_ptr *) ;
       (* The variable was bound to a functor. Print the functor and its
          components. *)
       BEGIN
        IF l <> NIL
         THEN
          IF tag_value(head(l)) = func
           THEN print_func(l)
           ELSE
            BEGIN
             list_printing := true ;
             write('[') ;
             print_components(l) ;
             write(']') ;
            END ;
       END ; (* print_a_list *)

      BEGIN
       IF list <> NIL
        THEN
         BEGIN
          print_bindings(tail(list)) ;
          IF pos('#',string_val(head(head(list)))) = 0
           THEN
            BEGIN
             printed := true ;
	     IF NOT no_wait THEN	(* MarkMLl *)
	       WriteLn
	     ELSE
               IF NOT solved THEN
                 WriteLn;
             write(string_val(head(head(list))),' = ') ;
             CASE tag_value(tail(head(list))) OF
              constant  : write(string_val(tail(head(list))),' ') ;
              variable  : print_variable(string_val(tail(head(list)))) ;
              cons_node : print_a_list(tail(head(list))) ;
             END ;
            END ;
         END ;
      END ; (* print_bindings *)

     BEGIN
      printed := false ;
      list_printing := false ;
      print_bindings(env) ;
      IF NOT printed
       THEN
        BEGIN
         writeln ;
         write('Yes ') ;
        END ;
      IF NOT no_wait THEN    		(* MarkMLl *)
        REPEAT
         read(kbd,ch) ;
        UNTIL ch IN [return,';']
      ELSE
        ch:= return;
      solved := (ch = return) ;
      writeln ;
     END ; (* check_continue *)

    FUNCTION copy_list(list : node_ptr ; copy_level : counter) : node_ptr ;
     (* Copy a list and append the copy_level (recursion level) to all
        variables. *)
     VAR
      temp_list,p : node_ptr ;
      level_str : string[6] ;

     PROCEDURE list_copy(from_list : node_ptr ; VAR to_list : node_ptr) ;
      BEGIN
       IF from_list <> NIL
        THEN
         CASE from_list^.tag OF
          variable : to_list := alloc_str(variable,
                                          concat(from_list^.string_data,
                                                 level_str)) ;
          func,
          constant  : to_list := from_list ;
          cons_node : BEGIN
                       list_copy(tail(from_list),to_list) ;
                       to_list := cons(copy_list(head(from_list),copy_level),
                                       to_list) ;
                      END ;
         END ;
      END ; (* list_copy *)

     BEGIN
      str(copy_level,level_str) ;
      level_str := concat('#',level_str) ;
      temp_list := NIL ;
      list_copy(list,temp_list) ;
      copy_list := temp_list ;
     END ; (* copy_list *)

    FUNCTION unify(list1,list2,environ : node_ptr ; VAR new_environ : node_ptr) :
                   boolean ;
     (* Unify two lists and return any new bindings at the front of the
        environment list. Returns true if the lists could be unified. This
        routine implements the unification table described in the article.
        Unification is straight forward, but the details of matching the
        lists get a little messy in this routine. There are better ways to
        do all of this, we just haven't gotten around to trying them. If
        you implement any other unification methods, we would be glad to
        hear about it.
        Unify checks to see if both lists are NIL, this is a successful
        unification. Otherwise check what kind on node the head of list1
        is and call the appropriate routine to perform the unification.
        Variables are unified by looking up the binding of the variable.
	If none is found, make a binding for the variable, otherwise try to
        unify the binding with list2. *)
     TYPE
      ur= (urFalse, urTrue, urUnset);
     VAR
      var_ptr : node_ptr ;
      unify_result: ur;
      unify_result_txt: STRING[16];

     FUNCTION urx(b: BOOLEAN): ur;
     BEGIN
       IF b THEN
	 urx:= urTrue
       ELSE
	 urx:= urFalse
     END (* urx *) ;

     PROCEDURE make_binding(l1,l2 : node_ptr) ;
      (* Bind a variable to the environment. Anonymous variables are not bound.
         l1 points to the variable and l2 points to its binding. *)
      BEGIN
       IF copy(string_val(l1),1,1) <> '_'
        THEN new_environ := cons(cons(l1,l2),environ)
        ELSE new_environ := environ ;
       unify_result := urTrue;
       unify_result_txt:= 'make_binding()';
      END ; (* make_binding *)

     PROCEDURE fail ;
      (* Unification failed. *)
      BEGIN
       unify_result := urFalse;
       unify_result_txt:= 'fail()';
       new_environ := environ ;
      END ; (* fail *)

     PROCEDURE unify_constant ;
      (* List1 contains a constant. Try to unify it with list2. The 4 cases
         are:
          list2 contains
           constant - unify if constants match
           variable - look up binding, if no current binding bind the
                      constant to the variable, otherwise unify list1
                      with the binding.
           cons_node,
           func     - these can't be unified with a constant. A cons_node
                      indicates an expression. *)

      PROCEDURE nil_constant ;
       BEGIN
        IF string_val(list1) = '[]'
         THEN
          BEGIN
	   unify_result := urTrue;
	   unify_result_txt:= 'nil_constant()';
           new_environ := environ ;
          END
         ELSE fail ;
       END ; (* nil_constant *)

      BEGIN
       IF list2 = NIL
        THEN nil_constant
        ELSE
         CASE tag_value(list2) OF
          constant  : IF string_val(list1) = string_val(list2)
                       THEN
                        BEGIN
			 unify_result := urTrue;
			 unify_result_txt:= 'unify_constant()';
                         new_environ := environ ;
                        END
                       ELSE fail ;
          variable  : BEGIN
                       var_ptr := look_up(string_val(list2),environ) ;
                       IF var_ptr = NIL
                        THEN make_binding(list2,list1)
                        ELSE
			 BEGIN
			  unify_result:= urx(unify(list1,var_ptr,environ,new_environ));
			  unify_result_txt:= 'unify_constant()'
			 END
                      END ;
          cons_node,
          func      : fail ;
          ELSE        fail ;
         END ;
      END ; (* unify_constant *)

     PROCEDURE unify_variable ;
      (* The first list contained a variable, now try to unify that variable
         with list2. If list2 is NIL, unify the varaible with '[]'. This
         is for printing purposes only. *)
      BEGIN
       var_ptr := look_up(string_val(list1),environ) ;
       IF var_ptr <> NIL
        THEN
	 BEGIN
	  unify_result:= urx(unify(var_ptr,list2,environ,new_environ));
	  unify_result_txt:= 'unify_variable()'
	 END
       ELSE IF list2 = NIL
        THEN make_binding(list1,alloc_str(constant,'[]'))
       ELSE IF tag_value(list2) IN [constant,variable,func,cons_node]
        THEN make_binding(list1,list2)
       ELSE fail ;
      END ; (* unify_variable *)

     PROCEDURE unify_func ;
      (* List1 contains a functor. Try to unify it with list2. The 4 cases
         are:
          list2 contains
           constant  - can't be unified.
           variable  - look up binding, if no current binding bind the
                       functor to the variable, otherwise unify list1
                       with the binding.
           cons_node - fail
           func      - if the functors match, then true to unify the component
                       lists (tail of the list) term by term. *)
      BEGIN
       CASE tag_value(list2) OF
        constant  : fail ;
        variable  : BEGIN
                     var_ptr := look_up(string_val(list2),environ) ;
                     IF var_ptr = NIL
                      THEN make_binding(list2,list1)
                      ELSE
		       BEGIN
			unify_result:= urx(unify(list1,var_ptr,environ,new_environ));
			unify_result_txt:= 'unify_func()'
		       END
                    END ;
        func      : IF string_val(list1) = string_val(list2)
                     THEN
                      BEGIN
		       unify_result := urTrue;
		       unify_result_txt:= 'unify_func()';
                       new_environ := environ ;
                      END
                     ELSE fail ;
        cons_node : fail ;
        ELSE        fail ;
       END ;
      END ; (* unify_func *)

     PROCEDURE unify_lists ;
      (* List1 contains an expression. Try to unify it with list2. The 4 cases
         are:
          list2 contains
           constant  - can't be unified.
           variable  - look up binding, if no current binding bind the
                       functor to the variable, otherwise unify list1
                       with the binding.
           cons_node - If the heads can be unified, then unify the tails.
           func      - fail *)
      BEGIN
       CASE tag_value(list2) OF
         constant  : fail ;
         variable  : BEGIN
                      var_ptr := look_up(string_val(list2),environ) ;
                      IF var_ptr = NIL
                       THEN make_binding(list2,list1)
                       ELSE
		        BEGIN
			 unify_result := urx(unify(list1,var_ptr,environ,new_environ));
			 unify_result_txt:= 'unify_lists()'
			END
                     END ;
         func      : fail ;
         cons_node : IF unify(head(list1),head(list2),environ,new_environ)
                      THEN
		       BEGIN
		        unify_result:= urx(unify(tail(list1),tail(list2),new_environ,
		                                          new_environ));
			unify_result_txt:= 'unify_lists()'
		       END
                      ELSE fail ;
         ELSE        fail ;
       END ;
      END ; (* unify_lists *)

     BEGIN
      unify_result:= urUnset;
      unify_result_txt:= 'unknown';
      IF do_debug THEN BEGIN		(* MarkMLl *)
	WriteLn('%% In unify()');
        Write('%% list1 -> ');
        print_list(list1);
        WriteLn;
        Write('%% list2 -> ');
        print_list(list2);
        WriteLn;
        Write('%% env -> ');
        print_list(environ);
        WriteLn
      END;
      IF (list1 = NIL) AND (list2 = NIL)
       THEN
        BEGIN
	 unify_result := urTrue;
	 unify_result_txt := 'unify()';
         new_environ := environ ;
        END
      ELSE IF list1 = NIL
       THEN
        BEGIN
	 unify_result := urx(unify(list2,list1,environ,new_environ));
	 unify_result_txt:= 'unify()'
	END
      ELSE
       CASE tag_value(list1) OF
        constant  : unify_constant ;
        variable  : unify_variable ;
        func      : unify_func ;
        cons_node : unify_lists ;
        ELSE        fail ;
       END ;
      CASE unify_result OF
	urFalse: unify:= FALSE;
	urTrue:  unify:= TRUE
      ELSE
	WriteLn;
	WriteLn('Internal error: unify() result unset.');
	WriteLn;
	HALT
      END;
      IF do_debug THEN BEGIN		(* MarkMLl *)
	Write('%% Leaving unify(), result set ');
	IF unify_result = urTrue THEN
	  Write('TRUE')
	ELSE
	  Write('FALSE');
	WriteLn(' by ', unify_result_txt);
        Write('%% new_env -> ');
        print_list(new_environ);
        WriteLn
      END
     END ; (* unify *)

    BEGIN
     saved_list := cons(list,cons(env,saved_list)) ;
     IF do_debug THEN BEGIN		(* MarkMLl *)
       WriteLn('%% In solve(), level ', level);
       Write('%% list -> ');
       print_list(list);
       WriteLn;
       Write('%% env -> ');
       print_list(env);
       WriteLn;
       IF do_debug3 THEN BEGIN
         Write('%% saved_list -> ');
         print_list(saved_list);
         WriteLn
       END
     END;
     IF list = NIL
      THEN check_continue
      ELSE
       BEGIN
        p := data_base ;
        WHILE (p <> NIL) AND (no_wait OR NOT solved) DO (* MarkMLl *)
         BEGIN
          test_memory ;
          IF unify(copy_list(head(head(p)),level),head(list),env,new_env)
           THEN solve(append_list(copy_list(tail(head(p)),level),tail(list)),
                      new_env,level + 1) ;
          p := tail(p) ;
         END ;
       END ;
     saved_list := tail(tail(saved_list)) ;
     IF do_debug THEN BEGIN		(* MarkMLl *)
       WriteLn('%% Leaving solve(), level ', level);
       IF do_debug3 THEN BEGIN
         Write('%% saved_list -> ');
         print_list(saved_list);
         WriteLn
       END
     END
    END ; (* solve *)

   BEGIN
    IF do_debug THEN			(* MarkMLl *)
      WriteLn('%% In query(), processing "', token, '"');
    q_ptr := NIL ;
    tail_list(q_ptr) ;
    IF do_debug THEN BEGIN		(* MarkMLl *)
      Write('%% After tail_list(q_ptr), q_ptr -> ');
      print_list(q_ptr);
      WriteLn
    END;
    IF token <> '.'
     THEN error('''.'' expected.')
    ELSE IF NOT error_flag
     THEN
      BEGIN
       solved := false ;
       saved_list := cons(data_base,NIL) ;
       IF do_debug THEN BEGIN		(* MarkMLl *)
         Write('%% About to call solve(), saved_list -> ');
         print_list(saved_list);
         WriteLn
       END;
       solve(q_ptr,NIL,0) ;
       IF NOT solved
        THEN writeln('No') ;
      END ;
   END ; (* query *)

  PROCEDURE read_new_file ;
   (* Read source statements from a new file. When all done, close file
      and continue reading from the old file. Files may be nested, but you
      will run into trouble if you nest them deaper than 15 levels. This
      is Turbo's default for open files. *)
   VAR
    new_file : text_file ;
    old_line,old_save : string132 ;
    f_name : string80 ;
   BEGIN
    IF token[1] = quote_char
     THEN delete(token,1,1) ;
    IF pos('.',token) = 0
     THEN f_name := concat(token,'.pro') (* Anticipate conversion to unix *)
     ELSE f_name := token ;		(* which is case-sensitive. MarkMLl *)
    IF open(new_file,f_name)
     THEN
      BEGIN
       old_line := line ;
       old_save := saved_line ;
       line := '' ;
       compile(new_file) ;
       close(new_file) ;
       line := old_line ;
       saved_line := old_save ;

  (* This is a hack. If reading file names from the command line then	*)
  (* don't expect a terminating . MarkMLl.				*)

       IF paramIndex < 0 THEN BEGIN
	 scan(source,token) ;
	 IF token <> '.'
          THEN error('''.'' expected.')
       END
      END
     ELSE BEGIN
       saved_line:= '';			(* Probably garbage. MarkMLl	*)
       error(concat('Unable to open ',f_name))
     END
   END ; (* read_new_file *)

  PROCEDURE do_exit ;
   (* Exit the program. This really should be a built-in function and handled
      in solve, but this does the trick. *)
   BEGIN
    scan(source,token) ;
    IF token <> '.'
     THEN error('''.'' expected.')
     ELSE halt
   END ; (* do_exit *)

  BEGIN

(* Assume that the only case in which the parameter is kbd is when	*)
(* being called from the body of the program, and use this to identify	*)
(* when command line parameters are to be processed. MarkMLl.		*)

   IF Addr(source) = Addr(kbd) THEN
     FOR paramIndex:= 1 TO ParamCount DO BEGIN
       token:= ParamStr(paramIndex);
       CASE classifyToken(token) OF
	 ttHelp,			(* These handled earlier	*)
	 ttVersion,
	 ttNoConsole: ;
	 ttWait:      no_wait:= FALSE;
	 ttNoWait:    no_wait:= TRUE;
	 ttEcho:      do_echo:= TRUE;
	 ttNoEcho:    do_echo:= FALSE;
	 ttDebug:     do_debug:= TRUE;
	 ttNoDebug:   do_debug:= FALSE;
	 ttDebug2:    do_debug2:= TRUE;
	 ttDebug3:    BEGIN
			do_debug2:= TRUE;
			do_debug3:= TRUE
		      END
       ELSE
         read_new_file		(* Recurses with this function		*)
       END
     END;
   paramIndex:= -1;		(* See read_new_file for significance	*)
   scan(source,token) ;
   WHILE token <> eof_mark DO
    BEGIN
     error_flag := false ;
     IF token = '?-'
      THEN
       BEGIN
        scan(source,token) ;
	query ;			(* Go do clever things			*)
       END
      ELSE IF token = '@'
       THEN
        BEGIN
         scan(source,token) ;
	 read_new_file ;	(* Recurses with this function		*)
        END
      ELSE IF toupper(token) = 'EXIT'
       THEN do_exit
      ELSE rule ;		(* Go do clever things			*)
    scan(source,token) ;
   END ;
  END ; (* compile *)

 PROCEDURE help;
 BEGIN
   writeln ;
   WriteLn('vtprolog [-no-console] [-[no-]echo] [-[no-]wait] [-[no-]debug] [file...]');
   writeln
 END (* help *) ;

 PROCEDURE banner;
 BEGIN
   writeln ;
   writeln('Very Tiny Prolog - Version 1.2    [c] 1986 MicroExpert Systems') ;
   writeln
 END (* banner *) ;

 PROCEDURE initialize ;
  (* Write a heading line and initialize the global variables *)
  VAR	i: INTEGER;
  BEGIN
   no_console:= FALSE;
   FOR i:= 1 TO ParamCount DO		(* Do this **FIRST**. MarkMLl.	*)
     CASE classifyToken(ParamStr(i)) OF
       ttNoConsole: BEGIN
		      no_console:= TRUE;
		      Assign(INPUT, '');
		      Reset(INPUT);
		      Assign(OUTPUT, '');
		      Rewrite(OUTPUT)
		    END
     ELSE
     END;
   no_wait:= FALSE;
   do_echo:= FALSE;
   do_debug:= FALSE;
   do_debug2:= FALSE;
   do_debug3:= FALSE;
   in_comment := false ;
   delim_set := [' ',')','(',',','[',']',eof_mark,tab,quote_char,':',
                 '@','.','?','|'] ;
   text_chars := [' ' .. '~'] ;
   line := '' ;
   data_base := NIL ;
   free := NIL ;
   saved_list := NIL ;
   total_free := 0.0 ;
   initial_heap := HeapPtr ;
   FOR i:= 1 TO ParamCount DO
     CASE classifyToken(ParamStr(i)) OF
       ttHelp:      BEGIN
		      help;
		      HALT
	            END;
       ttVersion:   BEGIN
		      banner;
		      HALT
		    END;
       ttDebug3:    BEGIN
		      WriteLn('initial_heap: ', HexA(initial_heap));
		      WriteLn('data_base: ', HexA(data_base));
		      WriteLn('free: ', HexA(free));
		      WriteLn('saved_list: ', HexA(saved_list));
		      do_debug3:= TRUE
		    END;
     ELSE
     END;

  (* Note new code above to optionally inhibit console initialisation	*)
  (* etc., this should allow the program to use output redirection when	*)
  (* compiled for Turbo Pascal v5.5. TP3 dislikes --no-console so we	*)
  (* have to hack things to see the initial values. MarkMLl.		*)

   IF (NOT no_console) AND (do_debug OR NOT do_debug3) THEN
     clrscr ;
   do_debug3:= FALSE;
   banner
  END ; (* initialize *)


 BEGIN
  initialize ;				(* Put no I/O before this. MarkMLl. *)

(* We can't handle files specified on the command line here since	*)
(* read_new_file is embedded in compile() rather than being mutually	*)
(* recursive. MarkMLl.							*)

  compile(kbd) ;
 END.
