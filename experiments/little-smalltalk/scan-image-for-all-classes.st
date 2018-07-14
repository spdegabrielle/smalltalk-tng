EVAL Class addNewClass: (Object subclass: 'Scavenger' variables: 'worklist fillPointer readPointer' classVariables: '')
META Scavenger
new
  ^ super new initialize
!
METHOD Scavenger
initialize
  worklist <- Array new: 10000.
  fillPointer <- 2. "simulates having already scheduled nil"
  readPointer <- 1.
!
METHOD Scavenger
schedule: anObject
  | c |
  c <- <2 anObject>.
  c == SmallInt ifTrue: [ ^self ].
  c == ByteArray ifTrue: [ ^self ].
  c == String ifTrue: [ ^self ].
  (self hasSeen: anObject) ifTrue: [ ^self ].
  worklist at: fillPointer put: anObject.
  fillPointer <- fillPointer + 1.
!
METHOD Scavenger
hasSeen: anObject
  1 to: (fillPointer - 1) do: [:i | ((worklist at: i) == anObject) ifTrue: [ ^true ]].
  ^ false
!
METHOD Scavenger
findAllClasses
  "self schedule: nil." "see Scavenger >> initialize"
  self schedule: true.
  self schedule: false.
  self schedule: Array.
  self schedule: Block.
  self schedule: Context.
  self schedule: SmallInt.
  self schedule: 'hello world'.
  [ readPointer < fillPointer ] whileTrue: [
    self scavenge: (worklist at: readPointer).
    readPointer <- readPointer + 1 ].
  Transcript show: 'Total objects scanned = ' + (fillPointer - 1).
!
METHOD Scavenger
scavenge: anObject
  | c slotCount |
  c <- <2 anObject>.
  (c class == Class) ifTrue: [
    Transcript show: 'Found classlike: ' + anObject printString.
    (Class classes includes: anObject) ifFalse: [Class addNewClass: anObject].
    ].
  self schedule: c.
  slotCount <- <4 anObject>.
  1 to: slotCount do: [:i | self schedule: (Object in: anObject at: i)].
!
EVAL Scavenger new findAllClasses
EVAL Class removeClass: Scavenger
