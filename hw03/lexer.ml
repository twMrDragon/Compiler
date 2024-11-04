open Format
open A

let split s =
  let len = String.length s in
  let b = { text = s; current = 0; last = -1 } in
  while true do
    let first = b.current in
    b.last <- -1;
    try A.start b
    with e ->
      if b.last = -1 then raise e;
      printf "--> \"%s\"\n" (String.sub s first (b.last - first));
      if b.last = len then (
        printf "reached end of file@.";
        exit 0);
      if b.last = first then (
        printf "would now loop@.";
        exit 1);
      b.current <- b.last
  done

let () = split (read_line ())
