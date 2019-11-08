| package |
package := Package name: 'quantum_tic_tac_toe'.
package paxVersion: 1;
	basicComment: ''.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAzIEYPDQAEAAAASW1hZ2VTdHJpcHBlcgAAAABSAAAAEwAAAHF1YW50dW1fdGljX3RhY190
b2VSAAAAPgAAAEM6XFVzZXJzXGRrdGVyXGJpZ19wcm9qZWN0c1xxdWFudHVtX3RpY190YWNfdG9l
XHF1YW50dW10dHQuZXhlmgAAAFIAAAATAAAAcXVhbnR1bV90aWNfdGFjX3RvZVIAAAAPAAAAUXVh
bnR1bVRUVFNoZWxs778lAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=').

package classNames
	add: #ButtonPresenter;
	add: #QuantumError;
	add: #QuantumTTTCell;
	add: #QuantumTTTGame;
	add: #QuantumTTTShell;
	add: #QuantumTTTTile;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Documents\Dolphin Smalltalk 7\Core\Contributions\ITC Gorisek\Dialect Abstraction Layer';
	add: '..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter';
	add: '..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	yourself).

package setManualPrerequisites: #(
	'Dialect Abstraction Layer').

package!

"Class Definitions"!

Object subclass: #QuantumTTTCell
	instanceVariableNames: 'tiles tile isClassical'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #QuantumTTTTile
	instanceVariableNames: 'symbol turn'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #QuantumError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #QuantumTTTGame
	instanceVariableNames: 'board'
	classVariableNames: 'BoardSize'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #QuantumTTTShell
	instanceVariableNames: 'currentSymbol currentTurn superposition cells cycle gameOver'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ValuePresenter subclass: #ButtonPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

QuantumTTTCell guid: (GUID fromString: '{7352411E-3C23-4D0B-AD0C-A29F07CBF118}')!
QuantumTTTCell comment: ''!
!QuantumTTTCell categoriesForClass!Kernel-Objects! !
!QuantumTTTCell methodsFor!

add: aTile
	isClassical ifFalse: [
		tiles add: aTile
	]
	ifTrue: [
		QuantumError signal: 'Cell is classical and cannot be changed'
	]!

cellString
	self isEmpty ifTrue: [^''].

	isClassical ifTrue: [
		^tile cellString
	]
	ifFalse: [
		| stream |
		stream := String new writeStream.
		stream nextPut: $[.
		tiles do: [:eachTile | stream nextPutAll: eachTile cellString]
			  separatedBy: [stream space].
		stream nextPut: $].
		^stream contents
	]!

clear
	isClassical := false.
	tiles := OrderedCollection new.
!

getOne
	^tiles at: 1!

hasTwoOrMoreStates
	^isClassical not and: [tiles size >= 2]!

includes: aTile
	isClassical ifTrue: [QuantumError signal: 'Cell is classical'].
	^tiles includes: aTile!

initialize
	tiles := OrderedCollection new.
	isClassical := false.!

isClassical
	^isClassical!

isEmpty
	^tiles isEmpty!

printOn: aStream
	isClassical ifTrue: [
		aStream nextPutAll: (tile cellString)
	]
	ifFalse: [
		aStream nextPutAll: $[.
		tiles do: [:eachTile | eachTile printOn: aStream ]
			  separatedBy: [aStream space].
		aStream nextPut: $].
	]!

reject: aBlock
	^self class with: (self tiles reject: aBlock)!

removeFirst
	^tiles removeFirst at: 1!

resolveTo: aTile
	tile := aTile.
	isClassical := true.!

tile
	^tile!

tiles
	isClassical ifTrue: [QuantumError signal: 'Cell is classical'].
	^tiles!

tiles: anOrderedCollection
	tiles := anOrderedCollection.! !
!QuantumTTTCell categoriesFor: #add:!public! !
!QuantumTTTCell categoriesFor: #cellString!public! !
!QuantumTTTCell categoriesFor: #clear!public! !
!QuantumTTTCell categoriesFor: #getOne!public! !
!QuantumTTTCell categoriesFor: #hasTwoOrMoreStates!public! !
!QuantumTTTCell categoriesFor: #includes:!public! !
!QuantumTTTCell categoriesFor: #initialize!public! !
!QuantumTTTCell categoriesFor: #isClassical!public! !
!QuantumTTTCell categoriesFor: #isEmpty!public! !
!QuantumTTTCell categoriesFor: #printOn:!public! !
!QuantumTTTCell categoriesFor: #reject:!public! !
!QuantumTTTCell categoriesFor: #removeFirst!public! !
!QuantumTTTCell categoriesFor: #resolveTo:!public! !
!QuantumTTTCell categoriesFor: #tile!public! !
!QuantumTTTCell categoriesFor: #tiles!public! !
!QuantumTTTCell categoriesFor: #tiles:!private! !

!QuantumTTTCell class methodsFor!

new
	^self basicNew initialize!

with: aTile
	^self new add: aTile; yourself!

withTiles: anOrderedCollection
	^self new; tiles: anOrderedCollection; yourself.! !
!QuantumTTTCell class categoriesFor: #new!public! !
!QuantumTTTCell class categoriesFor: #with:!public! !
!QuantumTTTCell class categoriesFor: #withTiles:!private! !

QuantumTTTTile guid: (GUID fromString: '{D6CF5A70-C3DC-484F-BF5B-D74D764FC1F9}')!
QuantumTTTTile comment: ''!
!QuantumTTTTile categoriesForClass!Kernel-Objects! !
!QuantumTTTTile methodsFor!

cellString
	^(symbol asString),(turn printString)!

printOn: aStream
	aStream nextPutAll: self cellString.!

symbol
	^symbol!

symbol: aSymbol
	symbol := aSymbol!

turn
	^turn!

turn: anInteger
	turn := anInteger! !
!QuantumTTTTile categoriesFor: #cellString!public! !
!QuantumTTTTile categoriesFor: #printOn:!public! !
!QuantumTTTTile categoriesFor: #symbol!public! !
!QuantumTTTTile categoriesFor: #symbol:!private! !
!QuantumTTTTile categoriesFor: #turn!public! !
!QuantumTTTTile categoriesFor: #turn:!private! !

!QuantumTTTTile class methodsFor!

symbol: aSymbol turn: anInteger
	^self new symbol: aSymbol; turn: anInteger; yourself! !
!QuantumTTTTile class categoriesFor: #symbol:turn:!public! !

QuantumError guid: (GUID fromString: '{B84323DF-C493-4C91-8415-16C271132C21}')!
QuantumError comment: ''!
!QuantumError categoriesForClass!Kernel-Exception Handling! !
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

checkGameOver
	"If someone won or 8+ cells are filled, the game is over"
	| filledCells |
	self checkThreeInARow ifTrue: [^true].
	
	filledCells := 0.
	board do: [:row |
		row do: [:cell |
			cell isClassical ifTrue: [filledCells := filledCells + 1]
		]
	].
	^filledCells >= 8
	!

checkThreeInARow
	"Check rows"
	board do: [:row |
		| rowFound |
		rowFound := (
			row conform: [:cell | cell isClassical and: [cell tile symbol = #X]]
		) or: [
			row conform: [:cell | cell isClassical and: [cell tile symbol = #O]]
		].
		
		rowFound ifTrue: [^true]
	].

	"Check cols"
	1 to: (board size) do: [:y |
		| colFound |
		colFound  := (
			(1 to: (board size)) conform: [:x |
				| cell |
				cell := self atX: x y: y.
				cell isClassical and: [cell tile symbol = #X]
			]
		) or: [
			(1 to: (board size)) conform: [:x |
				| cell |
				cell := self atX: x y: y.
				cell isClassical and: [cell tile symbol = #O]
			]
		].

		colFound ifTrue: [^true]
	].

	"Check diagonals"
	[
		| diagFound |
		diagFound := (((
			(1 to: (board size)) conform: [:x |
				| cell |
				cell := self atX: x y: x.
				cell isClassical and: [cell tile symbol = #X]
			]
		) or: [
			(1 to: (board size)) conform: [:x |
				| cell |
				cell := self atX: x y: x.
				cell isClassical and: [cell tile symbol = #O]
			]
		]) or: [
			(1 to: (board size)) conform: [:x |
				| cell |
				cell := self atX: x y: (board size - x + 1).
				cell isClassical and: [cell tile symbol = #O]
			]
		]) or: [
			(1 to: (board size)) conform: [:x |
				| cell |
				cell := self atX: x y: (board size - x + 1).
				cell isClassical and: [cell tile symbol = #O]
			]
		].

		diagFound ifTrue: [^true]
	] value.

	^false!

cyclicEntanglementStartingAtX: x y: y
	| initialCell initialTile initialPos currentTile currentPos done entangledPositions otherTries |
	initialPos := x @ y.
	initialCell := self atX: x y: y.
	initialCell hasTwoOrMoreStates ifFalse: [^nil].
	initialTile := initialCell getOne.
	entangledPositions := OrderedCollection new.
	otherTries := OrderedCollection new.
	currentTile := initialTile.
	currentPos := initialPos.
	done := false.
	[done] whileFalse: [
		| cell newCell |
		currentPos := self
					getSuperpositionPartnerOf: currentTile
					atX: currentPos x
					y: currentPos y.
		cell := self at: currentPos.
		cell hasTwoOrMoreStates ifFalse: [
			otherTries isEmpty
				ifTrue: [
					done := true.
					^nil
				]
				ifFalse: [
					"The search hit a dead end, time to backtrack"
					| state |
					self halt.
					state := otherTries removeFirst.
					cell := state at: 1.
					entangledPositions := state at: 2
				]
		].
		"If the cell has more than 2 states, pick one and add the rest to a queue, saving the state of entangledCells"
		newCell := cell reject: [:tile | tile = currentTile].
		currentTile := newCell removeFirst.
		newCell isEmpty ifFalse: [
			otherTries add: (Array with: newCell with: entangledPositions copy)
		].
		entangledPositions add: currentPos.
		currentTile = initialTile
			ifTrue: [
				done := true.
				^entangledPositions
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
			  and: [cell isClassical not])
			 and: [cell includes: tile]) ifTrue: [
				^ix@iy
			]
		]
	]!

initialize
	BoardSize := 3.
	board := Array ofSize: BoardSize.
	1 to: BoardSize do: [:y |
		board at: y put: (Array ofSize: BoardSize).
		1 to: BoardSize do: [:x |
			(board at: y) at: x put: (QuantumTTTCell new).
		]
	]!

place: tile at: pos
	^self place: tile x: (pos x) y: (pos x)!

place: tile x: x y: y
	((board at: y) at: x) add: tile.
	self trigger: #boardUpdated!

placeSuperposition: tile pos1: pos1 pos2: pos2
	self placeSuperposition: tile
		 x1: pos1 x
		 y1: pos1 y
		 x2: pos2 x
		 y2: pos2 y
	!

placeSuperposition: tile x1: x1 y1: y1 x2: x2 y2: y2
	self place: tile x: x1 y: y1.
	self place: tile x: x2 y: y2.
	!

put: tile at: pos
	^self put: tile x: (pos x) y: (pos y)!

put: tile x: x y: y
	((board at: y) at: x) resolveTo: tile.
	self trigger: #boardUpdated!

resolveCycle: aCyclicEntanglement atX: x y: y tile: aTile
	"Given a cyclic entanglement as a list of positions,
	resolve it so that aTile is a classical tile at (x, y)."

	| initialCell currentCell currentTile positions |
	initialCell := self atX: x y: y.
	currentCell := initialCell.
	currentTile := aTile.
	self
		put: aTile
		x: x
		y: y.
	positions := aCyclicEntanglement.
	
	[
		positions := positions reject: [:pos | (self at: pos) = currentCell or: [(self at: pos) isClassical]].
		positions isEmpty
	] whileFalse: [
		currentCell := (positions select: [:cell | (self at: cell) includes: currentTile]) at: 1.
		currentTile := ((self at: currentCell) tiles reject: [:tile | tile = currentTile]) at: 1.
		self put: currentTile at: currentCell
	].
	self resolveUnpairedSuperpositions!

resolveUnpairedSuperpositions
	"Private - Run after resolveCycle:atX:y:tile:
	If there is only one tile of a certain symbol and turn on the board,
	we know its position, so it can become a classical tile.
	If its pair is classical we know it's not there."
	| counts positions classical |
	counts := Dictionary new.
	positions := Dictionary new.
	classical := OrderedCollection new.

	board doWithIndex: [:row :y |
		row doWithIndex: [:cell :x |
			cell isClassical ifFalse: [
				cell tiles do: [:tile |
					counts at: tile put:
						(counts at: tile ifAbsent: [0]) + 1.
					positions at: tile put: x@y
				]
			]
			ifTrue: [
				classical add: (cell tile)
			]
		]
	].
	
	counts keysAndValuesDo: [:eachKey :eachValue |
		(eachValue = 1) ifTrue: [
			| pos |
			pos := positions at: eachKey.
			(classical includes: eachKey) ifFalse: [
				self put: eachKey at: pos
			]
			ifTrue: [
				(self at: pos) clear.
			]
		]
	]! !
!QuantumTTTGame categoriesFor: #at:!public! !
!QuantumTTTGame categoriesFor: #atX:y:!public! !
!QuantumTTTGame categoriesFor: #board!public! !
!QuantumTTTGame categoriesFor: #checkGameOver!public! !
!QuantumTTTGame categoriesFor: #checkThreeInARow!public! !
!QuantumTTTGame categoriesFor: #cyclicEntanglementStartingAtX:y:!public! !
!QuantumTTTGame categoriesFor: #findCyclicEntanglements!public! !
!QuantumTTTGame categoriesFor: #getSuperpositionPartnerOf:atX:y:!public! !
!QuantumTTTGame categoriesFor: #initialize!public! !
!QuantumTTTGame categoriesFor: #place:at:!private! !
!QuantumTTTGame categoriesFor: #place:x:y:!private! !
!QuantumTTTGame categoriesFor: #placeSuperposition:pos1:pos2:!public! !
!QuantumTTTGame categoriesFor: #placeSuperposition:x1:y1:x2:y2:!public! !
!QuantumTTTGame categoriesFor: #put:at:!private! !
!QuantumTTTGame categoriesFor: #put:x:y:!private! !
!QuantumTTTGame categoriesFor: #resolveCycle:atX:y:tile:!public! !
!QuantumTTTGame categoriesFor: #resolveUnpairedSuperpositions!private! !

QuantumTTTShell guid: (GUID fromString: '{9FE1F139-75DF-43A6-99BD-94AAC8701E0A}')!
QuantumTTTShell comment: ''!
!QuantumTTTShell categoriesForClass!MVP-Presenters! !
!QuantumTTTShell methodsFor!

addToSuperposition: aPoint
	superposition add: aPoint.
	superposition size = 2 ifTrue: [
		self placeSuperposition.
		self advanceTurn.
		superposition := OrderedCollection new.
		self checkCycle.
		self updateCells.
	]!

advanceTurn
	currentSymbol = #X ifTrue: [
		currentSymbol := #O
	]
	ifFalse: [
		currentSymbol := #X.
		currentTurn := currentTurn + 1.
	]!

checkCycle
	cycle := self model findCyclicEntanglements.!

checkGameOver
	"If someone won or 8+ cells are filled, the game is over"
	^self model checkGameOver!

createComponents
	super createComponents.
	currentSymbol := #X.
	currentTurn := 1.
	superposition := OrderedCollection new.
	cycle := nil.
	gameOver := false.
	cells := Array
		with: (Array with: 'cell11' with: 'cell12' with: 'cell13')
		with: (Array with: 'cell21' with: 'cell22' with: 'cell23')
		with: (Array with: 'cell31' with: 'cell32' with: 'cell33').
		!

model: aQuantumTTTGame
	super model: aQuantumTTTGame.

	self updateStatus!

onCellPress: aPoint
	cycle = nil ifTrue: [
		self addToSuperposition: aPoint.
	]
	ifFalse: [
		self resolveCycle: aPoint.
	].
	gameOver := self checkGameOver.
	self updateStatus!

onViewActivated: anEvent
	self updateStatus.
	^super onViewActivated: anEvent!

placeSuperposition
	| tile |
	tile := QuantumTTTTile symbol: currentSymbol turn: currentTurn.
	self model placeSuperposition: tile
			   pos1: (superposition at: 1)
			   pos2: (superposition at: 2)!

queryCommand: aCommandQuery
	super queryCommand: aCommandQuery.
	(aCommandQuery command asSymbol = #onCellPress:) ifTrue: [
		| pos cell |
		pos := aCommandQuery command arguments at: 1.
		cell := self model at: pos.

		"If the cell is classical, disable it since it can't be changed."
		aCommandQuery isEnabled: cell isClassical not.

		"If we're collapsing a cycle, disable the rest of the cells."
		cycle = nil ifFalse: [
			(cycle includes: pos) ifFalse: [
				aCommandQuery isEnabled: false.
			]
		].
		
		"If the game is over, disable everything."
		gameOver ifTrue: [aCommandQuery isEnabled: false].
	]!

resolveCycle: aPoint
	| tile |
	tile := ChoicePrompter choices: ((self model at: aPoint) tiles)
						   caption: 'Select a tile to collapse into'.
	self model resolveCycle: cycle atX: (aPoint x) y: (aPoint y) tile: tile.
	cycle := nil.
	self updateCells.!

updateCells
	self model board doWithIndex: [:row :iy |
		row doWithIndex: [:cell :ix |
			| btnName btn |
			btnName := (cells at: ix) at: iy.
			btn := self view viewNamed: btnName.
			btn text: (cell cellString).
		]
	]!

updateStatus
	| status stream |
	status := self view viewNamed: 'status'.
	stream := String new writeStream.
	
	gameOver ifTrue: [
		stream nextPutAll: 'Game over'.
	]
	ifFalse: [
		cycle = nil ifTrue: [
			stream
				nextPutAll: (currentSymbol asString);
				nextPutAll: '''s turn ';
				nextPutAll: (currentTurn printString);
				nextPutAll: '. Select two cells to play. (';
				nextPutAll: (superposition size printString);
				nextPutAll: '/2)'.
		]
		ifFalse: [
			stream
				nextPutAll: (currentSymbol asString);
				nextPutAll: '''s turn. Select a cell to collapse'
		].
	].

	status text: stream contents! !
!QuantumTTTShell categoriesFor: #addToSuperposition:!public! !
!QuantumTTTShell categoriesFor: #advanceTurn!public! !
!QuantumTTTShell categoriesFor: #checkCycle!public! !
!QuantumTTTShell categoriesFor: #checkGameOver!public! !
!QuantumTTTShell categoriesFor: #createComponents!private! !
!QuantumTTTShell categoriesFor: #model:!public! !
!QuantumTTTShell categoriesFor: #onCellPress:!public! !
!QuantumTTTShell categoriesFor: #onViewActivated:!public! !
!QuantumTTTShell categoriesFor: #placeSuperposition!public! !
!QuantumTTTShell categoriesFor: #queryCommand:!public! !
!QuantumTTTShell categoriesFor: #resolveCycle:!public! !
!QuantumTTTShell categoriesFor: #updateCells!public! !
!QuantumTTTShell categoriesFor: #updateStatus!public! !

!QuantumTTTShell class methodsFor!

defaultModel
	^QuantumTTTGame new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 26607617 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 661 771 551 0 0 0 416 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 234 256 98 0 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 530 3839 21 530 661 771 416 690 8 #updateMenuBar 608 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 201 8 0 0 139 1 0 0] 98 1 410 8 ##(Smalltalk.ContainerView) 98 15 0 416 98 2 8 1140850688 131073 880 0 0 0 7 0 0 0 880 0 234 256 98 2 410 8 ##(Smalltalk.StatusBar) 98 18 0 880 98 2 8 1409288204 1 992 0 482 8 4278190080 0 7 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 530 193 193 0 992 0 8 4294904727 234 256 98 2 410 8 ##(Smalltalk.StaticText) 98 16 0 992 98 2 8 1140850944 65 1248 0 0 0 7 0 0 0 1248 0 8 4294904859 852486 ##(Smalltalk.NullConverter) 0 0 0 626 202 208 98 2 690 720 98 2 530 1 1 530 631 37 1248 690 8 #text: 98 1 8 'Quantum Tic Tac Toe' 1248 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 59 1 0 0 18 0 0 0] 98 0 530 193 193 0 27 8 'status' 98 1 853766 ##(Smalltalk.StatusBarItem) 1 -1 992 0 459270 ##(Smalltalk.Message) 8 #displayString 98 0 1682 8 #iconImageIndex 98 0 1049926 1 ##(Smalltalk.IconImageManager) 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 992 0 562 1 1 1 626 202 208 98 1 690 720 98 2 530 1 653 530 661 41 992 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 70 1 0 0 74 1 0 0 90 1 0 0] 98 1 1248 1600 0 27 8 'statusBar' 0 626 202 208 98 1 690 720 98 2 530 1 1 530 661 693 880 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 74 1 0 0 90 1 0 0] 98 2 410 896 98 15 0 880 98 2 8 1140850688 131073 2208 0 0 0 7 0 0 0 2208 656390 ##(Smalltalk.GridLayout) 7 7 11 11 234 256 98 18 410 8 ##(Smalltalk.PushButton) 98 20 0 2208 98 2 8 1140924416 1 2336 0 0 0 7 0 0 0 2336 0 8 4294904739 1180998 4 ##(Smalltalk.CommandDescription) 1682 8 #onCellPress: 98 1 530 5 3 0 1 1 0 0 32 0 0 0 626 202 208 98 2 690 720 98 2 530 1 213 530 201 205 2336 690 8 #isEnabled: 98 1 32 2336 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 106 0 0 0 100 0 0 0 208 0 0 0] 98 0 1600 0 29 8 'cell21' 410 2352 98 20 0 2208 98 2 8 1140924416 1 2752 0 0 0 7 0 0 0 2752 0 8 4294904739 2434 1682 2480 98 1 530 7 7 0 1 1 0 0 32 0 0 0 626 202 208 98 2 690 720 98 2 530 421 427 530 201 203 2752 690 2656 98 1 32 2752 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 210 0 0 0 213 0 0 0 54 1 0 0 58 1 0 0] 98 0 1600 0 29 8 'cell33' 410 2352 98 20 0 2208 98 2 8 1140924416 1 3104 0 0 0 7 0 0 0 3104 0 8 4294904739 2434 1682 2480 98 1 530 5 7 0 1 1 0 0 32 0 0 0 626 202 208 98 2 690 720 98 2 530 421 213 530 201 205 3104 690 2656 98 1 32 3104 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 210 0 0 0 106 0 0 0 54 1 0 0 208 0 0 0] 98 0 1600 0 29 8 'cell23' 410 2352 98 20 0 2208 98 2 8 1140924416 1 3456 0 0 0 7 0 0 0 3456 0 8 4294904739 2434 1682 2480 98 1 530 3 3 0 1 1 0 0 32 0 0 0 626 202 208 98 2 690 720 98 2 530 1 1 530 201 203 3456 690 2656 98 1 32 3456 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 100 0 0 0 101 0 0 0] 98 0 1600 0 29 8 'cell11' 410 2352 98 20 0 2208 98 2 8 1140924416 1 3808 0 0 0 7 0 0 0 3808 0 8 4294904739 2434 1682 2480 98 1 530 7 3 0 1 1 0 0 32 0 0 0 626 202 208 98 2 690 720 98 2 530 1 427 530 201 203 3808 690 2656 98 1 32 3808 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 213 0 0 0 100 0 0 0 58 1 0 0] 98 0 1600 0 29 8 'cell31' 410 2352 98 20 0 2208 98 2 8 1140924416 1 4160 0 0 0 7 0 0 0 4160 0 8 4294904739 2434 1682 2480 98 1 530 5 5 0 1 1 0 0 32 0 0 0 626 202 208 98 2 690 720 98 2 530 211 213 530 201 205 4160 690 2656 98 1 32 4160 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 106 0 0 0 205 0 0 0 208 0 0 0] 98 0 1600 0 29 8 'cell22' 410 2352 98 20 0 2208 98 2 8 1140924416 1 4512 0 0 0 7 0 0 0 4512 0 8 4294904739 2434 1682 2480 98 1 530 7 5 0 1 1 0 0 32 0 0 0 626 202 208 98 2 690 720 98 2 530 211 427 530 201 203 4512 690 2656 98 1 32 4512 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 213 0 0 0 205 0 0 0 58 1 0 0] 98 0 1600 0 29 8 'cell32' 410 2352 98 20 0 2208 98 2 8 1140924416 1 4864 0 0 0 7 0 0 0 4864 0 8 4294904739 2434 1682 2480 98 1 530 3 7 0 1 1 0 0 32 0 0 0 626 202 208 98 2 690 720 98 2 530 421 1 530 201 203 4864 690 2656 98 1 32 4864 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 210 0 0 0 0 0 0 0 54 1 0 0 101 0 0 0] 98 0 1600 0 29 8 'cell13' 410 2352 98 20 0 2208 98 2 8 1140924416 1 5216 0 0 0 7 0 0 0 5216 0 8 4294904739 2434 1682 2480 98 1 530 3 5 0 1 1 0 0 32 0 0 0 626 202 208 98 2 690 720 98 2 530 211 1 530 201 203 5216 690 2656 98 1 32 5216 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 0 0 0 0 205 0 0 0 101 0 0 0] 98 0 1600 0 29 8 'cell12' 0 626 202 208 98 1 690 720 98 2 530 5 5 530 621 629 2208 818 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 2 0 0 0 56 1 0 0 60 1 0 0] 98 9 3456 5216 4864 2336 4160 3104 3808 4512 2752 1600 0 27 992 1600 0 27 1600 0 27 )! !
!QuantumTTTShell class categoriesFor: #defaultModel!public! !
!QuantumTTTShell class categoriesFor: #resource_Default_view!public!resources-views! !

ButtonPresenter guid: (GUID fromString: '{5041A1F5-4F4F-46B8-95FE-BF3817192CD9}')!
ButtonPresenter comment: ''!
!ButtonPresenter categoriesForClass!MVP-Presenters! !
!ButtonPresenter methodsFor!

onValueChanged
	self halt.
	self view text: self value.! !
!ButtonPresenter categoriesFor: #onValueChanged!public! !

"Binary Globals"!

