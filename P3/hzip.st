#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: cmdline.st,v 1.8 2011-05-05 20:45:21-07 - - $"
" Cesar Neri   
  ceneri@ucsc.edu
  1513805
  hzip.st
"
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
   "For each leaf, write out a 0 bit, followed by the 8 bits of
    the corresponding byte. Write out the bits in the order bit 7, 
    bit 6, . . ., bit 0, that is high bit first."
    |indexChar bit|
    outputFile writeBit: (0 bitAt: 1).
    indexChar := 8.
    [indexChar > 0] whileTrue: [
      bit := char value bitAt: indexChar.
      outputFile writeBit: bit.
      indexChar := indexChar - 1.
    ].
    "As a special case, if the byte is 0, write out bit 8, which
    will be a 0 for a byte value of 0, and 1 for a byte value of 
    256 (the EOF marker)."
    (char value = 0) 
    ifTrue: [ outputFile writeBit: (0 bitAt: 1). ].
    (char value = 256) 
    ifTrue: [ outputFile writeBit: (1 bitAt: 1). ].
  ]

]


Leaf subclass: Tree [
   |left right|
   left [ ^ left ]
   right [ ^ right ]

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
      byte isNil ifTrue: [ ^ nil ].
      ^byte value bitAt: bitIndex
   ]
   nextByte [
      |bitCount nxtByte|
      bitCount := 8.
      nxtByte := 0.
      [bitCount > 0] whileTrue: [
        nxtByte := nxtByte bitAt: bitCount put: self nextBit.
        bitCount := bitCount - 1.
      ].
      ^nxtByte.
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
   writeByte: byte [
      |bitCount nxtByte|
      bitCount := 8.
      [bitCount > 0] whileTrue: [
        self writeBit: (byte value bitAt: bitCount).
        bitCount := bitCount - 1.
      ].
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
stack := OrderedCollection new.

fileIn := (Smalltalk getArgv: 2).
fileOut := (Smalltalk getArgv: 3).


"Read in the input file and create a frequency table, counting the 
number of times each character appears on input (Using ZeroArray)"
c | t ifTrue:[
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

    "Add entry 256 with a count of 1 to indicate EOF"
    freqTable incr: 256.
    stdout flush.
].

"Iterate over the frequency table, and for each non-zero element, 
create a leaf node and insert that leaf node into a priority queue, 
with the character and the count."
c | t ifTrue:[
    freqTable keysAndValuesDo: [:key :value |
        (value notNil and: [value > 0]) ifTrue:[
            |char countval lNode|
            char := key asCharacter.
            countval := value.
            lNode := Leaf new: char count: countval.
            pQueue add: lNode.
        ]
    ].
].

"Print queue current status"
d ifTrue: [
    pQueue inspect.
].

"Generate Tree: Repeatedly remove the two smallest elements from the 
 priority queue, creating a new tree which is then entered into the 
 priority queue"
c | t ifTrue:[
  [pQueue size > 1] whileTrue:[
        | nLeft nRight newTree newChar newCount|
        nLeft := pQueue removeFirst.
        nRight := pQueue removeFirst.
        newChar := nLeft char.
        newCount := (nLeft count + nRight count).
        newTree := Tree new: newChar count: newCount 
          left: nLeft right: nRight.
        pQueue add: newTree.
  ].
]. 

"For each character that has appeared as non-zero in the frequency 
 table, construct an encoding string, using a depth-first traversal."
c | t ifTrue: [ 
    | huffmanTree |
    huffmanTree := pQueue first.
    huffmanTree inorder: [:char :string |
       newEncoding at: char asInteger put: string.
    ] prefix: ''.
].

"If the -t option is specified, write out the encoding table sorted by 
 character."
t ifTrue: [
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

"Print queue current status (The tree itself)"
d ifTrue: [
    stdout << pQueue first << nl.

].

"Compress"
c ifTrue: [
  [  | huffmanTree outputFile outBytes inFile nxtChar nxtCode|
    huffmanTree := pQueue first.
    (fileOut = nil)
    ifTrue: [
      outputFile := stdout.
    ]
    ifFalse: [
      outputFile := FileStream open: fileOut mode: FileStream write.
    ].
    "Write out the encoding table"
    outBytes := OutBits new: outputFile.
    huffmanTree postorderwrite: outBytes. 
    "When the tree is completely written out, write another 1 bit to
    indicate the end of the tree"
    outBytes writeBit: (1 bitAt: 1).
    "outBytes flushByte."

    "Reopen the input file and write out the encoded version of 
    each byte."
    inFile := FileStream open: fileIn mode: FileStream read.
    [inFile atEnd not] whileTrue: [
            nxtChar := inFile next asInteger.
            nxtCode := newEncoding at: nxtChar.
            nxtCode do: [:digitChar |
              outBytes writeBit: (digitChar value bitAt: 1). "0 or 1"
            ].
    ].
    "At end of the buffer, write out the encoding string for EOF. 
    Then pad with 0 bits out to a byte boundary, if needed."
    nxtCode := newEncoding at: 256.
    nxtCode do: [:digitChar |
      outBytes writeBit: (digitChar value bitAt: 1). "0 or 1"
    ].
    outBytes flushByte.

    inFile close.
    (outputFile isMemberOf: FileStream) ifTrue: [outputFile close.].

  ] on: SystemExceptions.FileError do: [:signal |
    |errno|
    errno := File errno.
    stdout << 'ERROR' << nl.
    stdout flush.
    stderr << execname << ': ' << filenameOut << ': '
          << signal messageText << nl.
    stderr flush.
  ].
].

"Uncompress"
u ifTrue: [
  [  | huffmanTree inFile inBytes outputFile outBytes isReadTreeDone
     break|
    "Initialize in file streams"
    inFile := FileStream open: fileIn mode: FileStream read.
    isReadTreeDone := false.
    "Recreate tree"
    inBytes := BitStream new: inFile.
    [(inBytes atEnd not)  & (isReadTreeDone not)] whileTrue: [
      |nxtBit nxtByte byteAsChar newLeaf |
      nxtBit := inBytes nextBit.
      "stdout << 'inside loop' << nl."
      "If it is a 0, read the next 8 bits and reconstruct the byte"
      (nxtBit = 0) 
      ifTrue: [
        "stdout << '0p' << nl."
        nxtByte := inBytes nextByte.
        byteAsChar :=  nxtByte asCharacter.
        "If the value is 0, read one more bit"
        (nxtByte = 0)
        ifTrue:[
          nxtBit := inBytes nextBit.
          (nxtBit = 1)
          ifTrue:[
            "if it is a 1, add 256 to the byte"
            byteAsChar := 256.
          ].
          
        ].
        "stdout << byteAsChar << nl."
        newLeaf := Leaf new: byteAsChar count: 0.
        "Then push this byte onto a stack."
        stack addFirst: newLeaf.
      ] 
      "If it is a 1, pop the 1-subtree from the stack, then pop the 
      0-subtree from the stack, and create a new tree with both of 
      those children and push the new tree back on the stack."
      ifFalse: [
        |subtree1 subtree0 newTree|
        "stdout << '1p' << nl."
        subtree1 := stack removeFirst.
        (stack size > 0)
        ifTrue: [
          subtree0 := stack removeFirst.
          newTree := Tree new: subtree0 char count: 0 
            left: subtree0 right: subtree1.
          stack addFirst: newTree.
        ]
        ifFalse: [
          stack addFirst: subtree1.
          isReadTreeDone := true. "Done with the tree"
        ].
      ].

    ].


    "OutputFile"
    (fileOut = nil)
    ifTrue: [
      outputFile := stdout.
    ]
    ifFalse: [
      outputFile := FileStream open: fileOut mode: FileStream write.
    ].
    outBytes := OutBits new: outputFile.

    "Now loop over the rest of the input file to reconstruct the 
    original file : Initialize a pointer to the root of the 
    decoding tree"
    huffmanTree := stack first.
    break := false.
    [(inBytes atEnd not) & (break not) ] whileTrue: [ 
      |nxtBit nxtTree charWrite |
      nxtTree := huffmanTree.

      [(nxtTree isMemberOf: Leaf) not] whileTrue: [
        nxtBit := inBytes nextBit.

        (nxtBit = 0)
        ifTrue: [ nxtTree := nxtTree left. ]
        ifFalse: [ nxtTree := nxtTree right. ].
      ]. "Exit loop nxtTree is character tree, write it"

      charWrite := nxtTree char.
      (charWrite = 256)
      ifTrue: [
        break := true. "Exit"
        [ ^ char ].
      ]
      ifFalse: [
        outBytes writeByte: charWrite.
      ].
    ].

    (outputFile isMemberOf: FileStream) ifTrue: [outputFile close.].
    inFile close.

  ] on: SystemExceptions.FileError do: [:signal |
    |errno|
    errno := File errno.
    stdout << 'ERROR' << nl.
    stdout flush.
    stderr << execname << ': ' << filenameOut << ': '
          << signal messageText << nl.
    stderr flush.
  ].

].
  
"Print tree"
d ifTrue: [
    stdout << stack first << nl.

].
