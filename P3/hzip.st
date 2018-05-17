#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: cmdline.st,v 1.8 2011-05-05 20:45:21-07 - - $"
""
""
""

execname := '$RCSfile: hzip.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.




"****************Utility Object Classes**************"
Character extend [
   isPrint [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [self asString]
                   ifFalse: [ ^ self asInteger printStringRadix: 16.]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   postorderwrite: outputFile [
   "for each leaf, write out a 0 bit, followed by the 8 bits of
    the corresponding byte. Write out the bits in the order bit 7, bit 6, . . ., bit 0,
    that is high bit first."
    |indexChar bit|
    indexChar := 8.
    [indexChar > 0] whileTrue: [
      bit := char value bitAt: indexChar.
      outputFile writeBit: bit.
      indexChar := indexChar - 1.
    ].
    "As a special case, if the byte is 0, write out bit 8, which
    will be a 0 for a byte value of 0, and 1 for a byte value of 256 (the EOF
    marker)."
    (char value = 0) 
    ifTrue: [outputFile writeBit: (0 bitAt: 1).].
    (char value = 256) 
    ifTrue: [outputFile writeBit: (1 bitAt: 1).].
  ]

]


Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   postorderwrite: outputFile [
      left  postorderwrite: outputFile.
      right  postorderwrite: outputFile.
      "For each interior node, write out a 1 bit."
      outputFile writeBit: (1 bitAt: 1).
   ]

]


Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

"**************MAIN***************************"

"Arguments passed"
d := false.
c := false.
t := false.
u := false.

argc:=0.
Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |

   (opt = $d) ifTrue: [ d := true. ].
   (opt = $t) ifTrue: [ t := true. ].
   (opt = $c) ifTrue: [ c := true. ].
   (opt = $u) ifTrue: [ u := true. ].
].  

"Variable declaration"
freqTable := ZeroArray new: 257.
newEncoding := ZeroArray new: 257.
pQueue := SortedCollection new.

fileIn := (Smalltalk getArgv: 2).
fileOut := (Smalltalk getArgv: 3).

"Create a new ZeroArray and count the frequency of characters."
true ifTrue:[
    |infile nxtchar|
    [infile := FileStream open: fileIn mode: FileStream read.
        [infile atEnd not] whileTrue: [
            nxtchar := infile next asInteger.
            freqTable incr: nxtchar.
        ].
        infile close.
    ] on: SystemExceptions.FileError do: [:signal |
    |errno|
    errno := File errno.
    stdout << 'ERROR' << nl.
    stdout flush.
    stderr << execname << ': ' << filenameOut << ': '
          << signal messageText << nl.
    stderr flush.
    ].

    "Add EOF"
    freqTable incr: 256.
    stdout flush.
].

"Generate all leaf nodes, and add them to Priority Queue"
c | t ifTrue:[
    freqTable keysAndValuesDo: [:key :value |
        (value notNil and: [value > 0]) ifTrue:[
            |char countval lNode|
            char := key asCharacter.
            countval := value.
            "stdout << '[' << char << ']=' << countval << nl."
            lNode := Leaf new: char count: countval.
            pQueue add: lNode.
        ]
    ].
].

"Debugging actions"
d ifTrue: [
    pQueue inspect.
].

"Generate Tree"
[pQueue size > 1] whileTrue:[
        | nLeft nRight newTree newChar newCount|
        nLeft := pQueue removeFirst.
        nRight := pQueue removeFirst.
        newChar := nLeft char.
        newCount := (nLeft count + nRight count).
        newTree := Tree new: newChar count: newCount left: nLeft right: nRight.
        pQueue add: newTree.
]. 

"Generate new encoding list"
c | t ifTrue: [ 
    | huffmanTree |
    huffmanTree := pQueue first.
    huffmanTree inorder: [:char :string |
       newEncoding at: char asInteger put: string.
    ] prefix: ''.
].

"Print encodifg table"
true ifTrue: [
    newEncoding keysAndValuesDo: [:key :value |
        (value notNil and: [value > 0]) ifTrue: [
            |character reps|
            (key asCharacter isPrint) 
            ifTrue: [ 
                character := (key asCharacter visible).
                "character asString size to: 3 do: [:skip| stdout]."
                stdout << ' ' << character << ' '. ]
            ifFalse: [
                (key = 256)
                ifTrue: [ stdout << 'EOF'.]
                ifFalse: [
                    character := (key asCharacter visible).
                    (character size > 4) 
                    ifTrue: [
                        character := 'x',  (character copyFrom: 4).
                        stdout << character. ]
                    ifFalse: [
                        character := 'x0',  (character copyFrom: 4).    
                        stdout << character. ]. 
                ].
            ].

            "Print repetitions" 
            reps := (freqTable at: key).
            reps asString size to: 5 do: [:skip| stdout << ' '].
            stdout << reps << ' '.
            "Print binary code"
            stdout << value << nl.
        ].
    ].
].


"Debugging actions"
d ifTrue: [
    "Only the tree should be there"
    "pQueue inspect."
    "Tree itself"
    stdout << pQueue first << nl.

].

"Compress"
true ifTrue: [
  | huffmanTree outputFile outBytes inFile nxtChar nxtCode|
  huffmanTree := pQueue first.
  (fileOut = nil)
  ifTrue: [
    outputFile := stdout.
  ]
  ifFalse: [
    outputFile := FileStream open: fileOut mode: FileStream write.
    "stdout << fileOut << nl."
  ].
  outBytes := OutBits new: outputFile.
  huffmanTree postorderwrite: outBytes. 
  "When the tree is completely written out, write another 1 bit to 
  indicate the end of the tree"
  outBytes writeBit: (1 bitAt: 1).
  outBytes flushByte.

  "Reopen the input file and write out the encoded version of each byte. "
  inFile := FileStream open: fileIn mode: FileStream read.
  [inFile atEnd not] whileTrue: [
          nxtChar := inFile next asInteger.
          nxtCode := newEncoding at: nxtChar.
          nxtCode do: [:digitChar |
            outBytes writeBit: (digitChar value bitAt: 1). "0 or 1"
          ].
  ].
  "At end of the buffer, write out the encoding string for EOF. Then pad 
   with 0 bits out to a byte boundary, if needed."
  nxtCode := newEncoding at: 256.
  nxtCode do: [:digitChar |
    outBytes writeBit: (digitChar value bitAt: 1). "0 or 1"
  ].
  outBytes flushByte.

  "!!Make sure it padds extra 0 to byte"

  inFile close.
  outputFile close.
].