#lang vanilla
# http://adventofcode.com/2016/day/1

map = Prelude.map
abs = Prelude.abs
foldl = Prelude.foldl
replicate = Prelude.replicate
elem = Prelude.elem
assert = Prelude.assert

main = (input) ->
  let inp = parse(input) in
  show([
    measure(interp(initialState, inp)),
    # trace returns the list of each state we were at after each instructionf
    # map stateLoc extracts just the position information.
    # uniqAdjacent removes duplicates that result from "L" and "R" instructions.
    # findFirstDup finds the first position we visited twice.
    measure(findFirstDup(uniqAdjacent(map(stateLoc, trace(initialState, inp))))),
  ])


measure = (state) ->
  abs(first(state)) + abs(first(rest(state)))

initialState = [0, 0, "N"]

stateLoc = (s) -> [first(s), first(rest(s))]

# note foldl uses Haskell's argument order, not Scheme's
interp = (state, instructions) -> foldl(step, state, instructions)
trace = (state, instructions) ->
  if isEmpty(instructions) then [state]
  else let newState = step(state, first(instructions)) in
         cons(state, trace(newState, rest(instructions)))

step = (state, instruction) ->
  if      instruction == "R" then turnR(state)
  else if instruction == "L" then turnL(state)
  else let x   = first(state) in
       let y   = first(rest(state)) in
       let dir = first(rest(rest(state))) in
       let i   = instruction in
         if      dir == "N" then [x,     y + i, dir]
         else if dir == "S" then [x,     y - i, dir]
         else if dir == "E" then [x + i, y,     dir]
         else if dir == "W" then [x - i, y,     dir]
         else error("bad dir")

turnR = (state) ->
  let x   = first(state) in
  let y   = first(rest(state)) in
  let dir = first(rest(rest(state))) in
  if      dir == "N" then [x, y, "E"]
  else if dir == "E" then [x, y, "S"]
  else if dir == "S" then [x, y, "W"]
  else if dir == "W" then [x, y, "N"]
  else error("bad dir")

turnL = (state) ->
  let x   = first(state) in
  let y   = first(rest(state)) in
  let dir = first(rest(rest(state))) in
  if      dir == "N" then [x, y, "W"]
  else if dir == "W" then [x, y, "S"]
  else if dir == "S" then [x, y, "E"]
  else if dir == "E" then [x, y, "N"]
  else error("bad dir")


parse = (str) ->  # list of instructions
  let toks = split(str, ", ") in
  concat(map(parseTok, toks))

parseTok = (tok) ->
  # Instead of parsing R3 as ["R", 3], parse it as ["R", 1, 1, 1].
  # It's important to consider every 1-unit step separately,
  # because the set of "seen" locations has to count every point we pass through.
  cons(slice(tok, 0, 1), replicate(parseInt(slice(tok, 1, length(tok))), 1))

findFirstDup = (lst) -> dupLoop([], lst)
dupLoop = (seen, lst) ->
  if isEmpty(lst) then error("no duplicate found")
  else if elem(first(lst), seen) then first(lst)
  else dupLoop(cons(first(lst), seen), rest(lst))

uniqAdjacent = (lst) ->
  if isEmpty(lst) then lst
  else if isEmpty(rest(lst)) then lst
  else if first(lst) == first(rest(lst)) then uniqAdjacent(cons(first(lst), rest(rest(lst))))
  else cons(first(lst), uniqAdjacent(rest(lst)))


exampleInput = "L1, L5, R1, R3, L4, L5, R5, R1, L2, L2, L3, R4, L2, R3, R1, L2, R5, R3, L4, R4, L3, R3, R3, L2, R1, L3, R2, L1, R4, L2, R4, L4, R5, L3, R1, R1, L1, L3, L2, R1, R3, R2, L1, R4, L4, R2, L189, L4, R5, R3, L1, R47, R4, R1, R3, L3, L3, L2, R70, L1, R4, R185, R5, L4, L5, R4, L1, L4, R5, L3, R2, R3, L5, L3, R5, L1, R5, L4, R1, R2, L2, L5, L2, R4, L3, R5, R1, L5, L4, L3, R4, L3, L4, L1, L5, L5, R5, L5, L2, L1, L2, L4, L1, L2, R3, R1, R1, L2, L5, R2, L3, L5, L4, L2, L1, L2, R3, L1, L4, R3, R3, L2, R5, L1, L3, L3, L3, L5, R5, R1, R2, L3, L2, R4, R1, R1, R3, R4, R3, L3, R3, L5, R2, L2, R4, R5, L4, L3, L1, L5, L1, R1, R2, L1, R3, R4, R5, R2, R3, L2, L1, L5"
assert(==, main(exampleInput), "[253, 126]")
