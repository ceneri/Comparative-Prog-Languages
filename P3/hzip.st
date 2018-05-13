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

"Create a new ZeroArray and count the frequency of characters."
true ifTrue:[
    |infile nxtchar|
    [infile := FileStream open: fileIn mode: FileStream read.
        [infile atEnd not] whileTrue: [
            nxtchar := infile next asInteger.
            freqTable incr: nxtchar.
        ].
        nxtchar = nil ifFalse: [
            freqTable incr: 256.
        ].
        infile close.
    ] on: SystemExceptions.FileError do: [:signal |
    |errno|
    errno := File errno.
    stdout flush.
    stderr << execname << ': ' << filenameOut << ': '
          << signal messageText << nl.
    stderr flush.
    ].
].

"Generate all leaf nodes, and add them to Priority Queue"
t ifTrue:[
    freqTable keysAndValuesDo: [:key :value |
        (value notNil) ifTrue:[
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
t ifTrue: [ 
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
                character asString size to: 1 do: [:skip| stdout << ' '].
                stdout << character << '  '. ]
            ifFalse: [
                (key = 256)
                ifTrue: [ stdout << 'EOF' << ' '.]
                ifFalse: [
                    character := (key asCharacter visible).
                    (character size > 4) 
                    ifTrue: [
                        character := 'x',  (character subStrings: '16r') first.
                        stdout << character << ' '. ]
                    ifFalse: [
                        character := 'x0',  (character subStrings: '16r') first.
                        stdout << character << ' '. ] 
                    ].
            ].
             
            reps := (freqTable at: key).
            reps asString size to: 6 do: [:skip| stdout << ' '].
            stdout << reps << '  '.
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