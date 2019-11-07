| package |
package := Package name: 'quantum_tic_tac_toe'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #QuantumTTTGame;
	add: #QuantumTTTTile;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #QuantumTTTTile
	instanceVariableNames: 'symbol turn'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #QuantumTTTGame
	instanceVariableNames: 'board'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

QuantumTTTTile guid: (GUID fromString: '{D6CF5A70-C3DC-484F-BF5B-D74D764FC1F9}')!
QuantumTTTTile comment: ''!
!QuantumTTTTile categoriesForClass!Kernel-Objects! !
!QuantumTTTTile methodsFor!

printOn: aStream
	aStream nextPutAll: 'a QuantumTTTTile('.
	symbol printOn: aStream.
	aStream  nextPutAll: ', turn '.
	turn printOn: aStream.
	aStream nextPutAll: ')'!

printString
	^'a QuantumTTTTile(' , (symbol printString) , ', turn ' , (turn printString) , ')'!

symbol
	^symbol!

symbol: aSymbol
	symbol := aSymbol!

turn
	^turn!

turn: anInteger
	turn := anInteger! !
!QuantumTTTTile categoriesFor: #printOn:!public! !
!QuantumTTTTile categoriesFor: #printString!public! !
!QuantumTTTTile categoriesFor: #symbol!public! !
!QuantumTTTTile categoriesFor: #symbol:!private! !
!QuantumTTTTile categoriesFor: #turn!public! !
!QuantumTTTTile categoriesFor: #turn:!private! !

!QuantumTTTTile class methodsFor!

symbol: aSymbol turn: anInteger
	^self new symbol: aSymbol; turn: anInteger; yourself! !
!QuantumTTTTile class categoriesFor: #symbol:turn:!public! !

QuantumTTTGame guid: (GUID fromString: '{455FD1A1-2BD7-4F4B-84BF-AFEAD5234E2B}')!
QuantumTTTGame comment: ''!
!QuantumTTTGame categoriesForClass!MVP-Models! !
!QuantumTTTGame methodsFor!

at: pos
	^self atX: (pos x) y: (pos y)!

atX: x y: y
	^(board at: y) at: x!

board
	^board!

cyclicEntanglementStartingAtX: x y: y
	| initialCell initialTile initialPos currentTile currentPos done entangledCells otherTries |
	initialPos := x@y.
	initialCell := self atX: x y: y.
	(self class isCellSuperposition: initialCell) ifFalse: [^nil].
	initialTile := initialCell at: 1.
	entangledCells := OrderedCollection new.
	otherTries := OrderedCollection new.

	currentTile := initialTile.
	currentPos := initialPos.
	done := false.
	[done] whileFalse: [
		| cell tiles |
		currentPos := self getSuperpositionPartnerOf: currentTile atX: (currentPos x) y: (currentPos y).
		cell := self at: currentPos.
		(self class isCellSuperposition: cell) ifFalse: [
			(otherTries isEmpty) ifTrue: [
				done := true.
				^nil
			]
			ifFalse: [
				"The search hit a dead end, time to backtrack"
				| state |
				state := otherTries removeFirst.
				cell := state at: 1.
				entangledCells := state at: 2.
			]
		].
		"If the cell has more than 2 states, pick one and add the rest to a queue, saving the state of entangledCells"
		tiles := (cell reject: [:tile | tile = currentTile]).
		currentTile := tiles removeFirst.
		(tiles isEmpty) ifFalse: [
			otherTries add: (Array with: tiles with: (entangledCells copy)).
		].

		entangledCells add: currentPos.
		(currentTile = initialTile) ifTrue: [
			done := true.
			^entangledCells
		]
	]!

findCyclicEntanglements
	1 to: (board size) do: [:y | 
		1 to: ((board at: 1) size) do: [:x |
			| entanglement |
			entanglement := self cyclicEntanglementStartingAtX: x y: y.
			(entanglement = nil) ifFalse: [^entanglement]
		]
	].
	^nil!

getSuperpositionPartnerOf: tile atX: x y: y
	board doWithIndex: [ :row :iy |
		row doWithIndex: [ :cell :ix |
			((((ix ~= x) | (iy ~= y))
			  and: [cell isMemberOf: OrderedCollection])
			 and: [cell includes: tile]) ifTrue: [
				^ix@iy
			]
		]
	]!

initialize
	board := Array with: (Array ofSize: 3)
		       with: (Array ofSize: 3)
		       with: (Array ofSize: 3).!

place: tile at: pos
	^self place: tile x: (pos x) y: (pos x)!

place: tile x: x y: y
	((self atX: x y: y) isMemberOf: OrderedCollection) ifTrue: [
		((board at: y) at: x) addLast: tile
	]
	ifFalse: [
		(board at: y) at: x put: (OrderedCollection with: tile)
	]!

placeSuperposition: tile x1: x1 y1: y1 x2: x2 y2: y2
	self place: tile x: x1 y: y1.
	self place: tile x: x2 y: y2.
	!

put: tile at: pos
	^self put: tile x: (pos x) y: (pos y)!

put: tile x: x y: y
	(board at: y) at: x put: tile!

resolveCycle: aCyclicEntanglement atX: x y: y tile: aTile
	"Given a cyclic entanglement as a list of positions,
	resolve it so that aTile is a classical tile at (x, y)."
	| initialCell currentCell currentTile cells |
	initialCell := self atX: x y: y.
	currentCell := initialCell.
	currentTile := aTile.
	self place: aTile x: x y: y.
	cells := aCyclicEntanglement.

	[
		cells := cells reject: [:cell | cell = currentCell].
		cells isEmpty
	] whileFalse: [
		currentCell := (cells select: [:cell | (self at: cell) includes: currentTile]) at: 1.
		currentTile := ((self at: currentCell) reject: [:tile | tile = currentTile]) at: 1.
		self put: currentTile at: currentCell
	].

	self resolveUnpairedSuperpositions!

resolveUnpairedSuperpositions
	"Private - Run after resolveCycle:atX:y:tile:
	If there is only one tile of a certain symbol and turn on the board,
	we know its position, so it can become a classical tile."
	| counts positions |
	counts := Dictionary new.
	positions := Dictionary new.

	board doWithIndex: [:row :y |
		row doWithIndex: [:cell :x |
			(cell isMemberOf: OrderedCollection) ifTrue: [
				cell do: [:tile |
					counts at: tile put:
						(counts at: tile ifAbsent: [0]) + 1.
					positions at: tile put: x@y
				]
			]
		]
	].
	
	counts keysAndValuesDo: [:eachKey :eachValue |
		(eachValue = 1) ifTrue: [
			| pos |
			pos := positions at: eachKey.
			self put: eachKey at: pos
		]
	]! !
!QuantumTTTGame categoriesFor: #at:!public! !
!QuantumTTTGame categoriesFor: #atX:y:!public! !
!QuantumTTTGame categoriesFor: #board!public! !
!QuantumTTTGame categoriesFor: #cyclicEntanglementStartingAtX:y:!public! !
!QuantumTTTGame categoriesFor: #findCyclicEntanglements!public! !
!QuantumTTTGame categoriesFor: #getSuperpositionPartnerOf:atX:y:!public! !
!QuantumTTTGame categoriesFor: #initialize!public! !
!QuantumTTTGame categoriesFor: #place:at:!private! !
!QuantumTTTGame categoriesFor: #place:x:y:!private! !
!QuantumTTTGame categoriesFor: #placeSuperposition:x1:y1:x2:y2:!public! !
!QuantumTTTGame categoriesFor: #put:at:!private! !
!QuantumTTTGame categoriesFor: #put:x:y:!private! !
!QuantumTTTGame categoriesFor: #resolveCycle:atX:y:tile:!public! !
!QuantumTTTGame categoriesFor: #resolveUnpairedSuperpositions!private! !

!QuantumTTTGame class methodsFor!

isCellSuperposition: aCell
	^(aCell isMemberOf: OrderedCollection) and: [aCell size >= 2]!

isCellSuperpositionOf2Tiles: aCell
	^(aCell isMemberOf: OrderedCollection) and: [
		aCell size = 2
	]! !
!QuantumTTTGame class categoriesFor: #isCellSuperposition:!public! !
!QuantumTTTGame class categoriesFor: #isCellSuperpositionOf2Tiles:!public! !

"Binary Globals"!

