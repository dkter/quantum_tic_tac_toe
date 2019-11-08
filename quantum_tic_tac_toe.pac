| package |
package := Package name: 'quantum_tic_tac_toe'.
package paxVersion: 1;
	basicComment: ''.


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
	add: '..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter';
	add: '..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

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
	instanceVariableNames: 'currentSymbol currentTurn superposition cells cycle'
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
		aStream nextPutAll: '!!('.
		tile printOn: aStream.
		aStream nextPut: $).
	]
	ifFalse: [
		aStream nextPutAll: '?('.
		tiles do: [:eachTile | eachTile printOn: aStream ]
			  separatedBy: [aStream space].
		aStream nextPut: $).
	]!

resolveTo: aTile
	tile := aTile.
	isClassical := true.!

tiles
	isClassical ifTrue: [QuantumError signal: 'Cell is classical'].
	^tiles! !
!QuantumTTTCell categoriesFor: #add:!public! !
!QuantumTTTCell categoriesFor: #cellString!public! !
!QuantumTTTCell categoriesFor: #getOne!public! !
!QuantumTTTCell categoriesFor: #hasTwoOrMoreStates!public! !
!QuantumTTTCell categoriesFor: #includes:!public! !
!QuantumTTTCell categoriesFor: #initialize!public! !
!QuantumTTTCell categoriesFor: #isClassical!public! !
!QuantumTTTCell categoriesFor: #isEmpty!public! !
!QuantumTTTCell categoriesFor: #printOn:!public! !
!QuantumTTTCell categoriesFor: #resolveTo:!public! !
!QuantumTTTCell categoriesFor: #tiles!public! !

!QuantumTTTCell class methodsFor!

new
	^self basicNew initialize!

with: aTile
	^self new add: aTile; yourself! !
!QuantumTTTCell class categoriesFor: #new!public! !
!QuantumTTTCell class categoriesFor: #with:!public! !

QuantumTTTTile guid: (GUID fromString: '{D6CF5A70-C3DC-484F-BF5B-D74D764FC1F9}')!
QuantumTTTTile comment: ''!
!QuantumTTTTile categoriesForClass!Kernel-Objects! !
!QuantumTTTTile methodsFor!

cellString
	^(symbol asString),(turn printString)!

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
!QuantumTTTTile categoriesFor: #cellString!public! !
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

cyclicEntanglementStartingAtX: x y: y
	| initialCell initialTile initialPos currentTile currentPos done entangledCells otherTries |
	initialPos := x@y.
	initialCell := self atX: x y: y.
	initialCell hasTwoOrMoreStates ifFalse: [^nil].
	initialTile := initialCell getOne.
	entangledCells := OrderedCollection new.
	otherTries := OrderedCollection new.

	currentTile := initialTile.
	currentPos := initialPos.
	done := false.
	[done] whileFalse: [
		| cell tiles |
		currentPos := self getSuperpositionPartnerOf: currentTile atX: (currentPos x) y: (currentPos y).
		cell := self at: currentPos.
		cell hasTwoOrMoreStates ifFalse: [
			otherTries isEmpty ifTrue: [
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
		tiles := (cell tiles reject: [:tile | tile = currentTile]).
		currentTile := tiles removeFirst.
		tiles isEmpty ifFalse: [
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
		place: aTile
		x: x
		y: y.
	positions := aCyclicEntanglement.
	
	[
		positions := positions reject: [:cell | cell = currentCell].
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
	we know its position, so it can become a classical tile."
	| counts positions |
	counts := Dictionary new.
	positions := Dictionary new.

	board doWithIndex: [:row :y |
		row doWithIndex: [:cell :x |
			cell isClassical ifFalse: [
				cell tiles do: [:tile |
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

createComponents
	super createComponents.
	currentSymbol := #X.
	currentTurn := 1.
	superposition := OrderedCollection new.
	cycle := nil.
	cells := Array
		with: (Array with: 'cell11' with: 'cell12' with: 'cell13')
		with: (Array with: 'cell21' with: 'cell22' with: 'cell23')
		with: (Array with: 'cell31' with: 'cell32' with: 'cell33').
		!

model: aQuantumTTTGame
	super model: aQuantumTTTGame.!

onCellPress: aPoint
	cycle = nil ifTrue: [
		self addToSuperposition: aPoint.
	]
	ifFalse: [
		self resolveCycle: aPoint.
	]!

placeSuperposition
	| tile |
	tile := QuantumTTTTile symbol: currentSymbol turn: currentTurn.
	self model placeSuperposition: tile
			   pos1: (superposition at: 1)
			   pos2: (superposition at: 2)!

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
			btn isEnabled: (cell isClassical).
		]
	]! !
!QuantumTTTShell categoriesFor: #addToSuperposition:!public! !
!QuantumTTTShell categoriesFor: #advanceTurn!public! !
!QuantumTTTShell categoriesFor: #checkCycle!public! !
!QuantumTTTShell categoriesFor: #createComponents!private! !
!QuantumTTTShell categoriesFor: #model:!public! !
!QuantumTTTShell categoriesFor: #onCellPress:!public! !
!QuantumTTTShell categoriesFor: #placeSuperposition!public! !
!QuantumTTTShell categoriesFor: #resolveCycle:!public! !
!QuantumTTTShell categoriesFor: #updateCells!public! !

!QuantumTTTShell class methodsFor!

defaultModel
	^QuantumTTTGame new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 26607617 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 651 691 551 0 0 0 416 0 234 256 98 18 410 8 ##(Smalltalk.PushButton) 98 20 0 416 98 2 8 1140924416 1 592 0 0 0 7 0 0 0 592 0 8 4294911001 1180998 4 ##(Smalltalk.CommandDescription) 459270 ##(Smalltalk.Message) 8 #onCellPress: 98 1 530 7 5 0 1 1 0 0 32 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 530 221 421 530 181 181 592 866 8 #isEnabled: 98 1 32 592 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 110 0 0 0 210 0 0 0 200 0 0 0 44 1 0 0] 98 0 530 193 193 0 29 8 'cell32' 410 608 98 20 0 416 98 2 8 1140924416 1 1104 0 0 0 7 0 0 0 1104 0 8 4294911001 690 722 752 98 1 530 3 5 0 1 1 0 0 32 0 0 0 802 202 208 98 2 866 896 98 2 530 221 21 530 181 181 1104 866 976 98 1 32 1104 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 110 0 0 0 10 0 0 0 200 0 0 0 100 0 0 0] 98 0 1072 0 29 8 'cell12' 410 608 98 20 0 416 98 2 8 1140924416 1 1456 0 0 0 7 0 0 0 1456 0 8 4294911001 690 722 752 98 1 530 3 7 0 1 1 0 0 32 0 0 0 802 202 208 98 2 866 896 98 2 530 421 21 530 181 181 1456 866 976 98 1 32 1456 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 210 0 0 0 10 0 0 0 44 1 0 0 100 0 0 0] 98 0 1072 0 29 8 'cell13' 410 608 98 20 0 416 98 2 8 1140924416 1 1808 0 0 0 7 0 0 0 1808 0 8 4294911001 690 722 752 98 1 530 5 5 0 1 1 0 0 32 0 0 0 802 202 208 98 2 866 896 98 2 530 221 217 530 181 181 1808 866 976 98 1 32 1808 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 110 0 0 0 108 0 0 0 200 0 0 0 198 0 0 0] 98 0 1072 0 29 8 'cell22' 410 608 98 20 0 416 98 2 8 1140924416 1 2160 0 0 0 7 0 0 0 2160 0 8 4294911001 690 722 752 98 1 530 3 3 0 1 1 0 0 32 0 0 0 802 202 208 98 2 866 896 98 2 530 17 19 530 181 181 2160 866 976 98 1 32 2160 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 9 0 0 0 98 0 0 0 99 0 0 0] 98 0 1072 0 29 8 'cell11' 410 608 98 20 0 416 98 2 8 1140924416 1 2512 0 0 0 7 0 0 0 2512 0 8 4294911001 690 722 752 98 1 530 5 3 0 1 1 0 0 32 0 0 0 802 202 208 98 2 866 896 98 2 530 21 221 530 181 181 2512 866 976 98 1 32 2512 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 110 0 0 0 100 0 0 0 200 0 0 0] 98 0 1072 0 29 8 'cell21' 410 608 98 20 0 416 98 2 8 1140924416 1 2864 0 0 0 7 0 0 0 2864 0 8 4294911001 690 722 752 98 1 530 5 7 0 1 1 0 0 32 0 0 0 802 202 208 98 2 866 896 98 2 530 421 221 530 181 181 2864 866 976 98 1 32 2864 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 210 0 0 0 110 0 0 0 44 1 0 0 200 0 0 0] 98 0 1072 0 29 8 'cell23' 410 608 98 20 0 416 98 2 8 1140924416 1 3216 0 0 0 7 0 0 0 3216 0 8 4294911001 690 722 752 98 1 530 7 7 0 1 1 0 0 32 0 0 0 802 202 208 98 2 866 896 98 2 530 421 417 530 181 181 3216 866 976 98 1 32 3216 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 210 0 0 0 208 0 0 0 44 1 0 0 42 1 0 0] 98 0 1072 0 29 8 'cell33' 410 608 98 20 0 416 98 2 8 1140924416 1 3568 0 0 0 7 0 0 0 3568 0 8 4294911001 690 722 752 98 1 530 7 3 0 1 1 0 0 32 0 0 0 802 202 208 98 2 866 896 98 2 530 21 421 530 181 181 3568 866 976 98 1 32 3568 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 210 0 0 0 100 0 0 0 44 1 0 0] 98 0 1072 0 29 8 'cell31' 0 0 0 0 0 1 0 0 0 0 1 0 0 802 202 208 98 2 866 896 98 2 530 3839 21 530 651 691 416 866 8 #updateMenuBar 98 0 416 1010 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 196 8 0 0 99 1 0 0] 98 9 2160 1104 1456 2512 1808 2864 3568 592 3216 1072 0 27 )! !
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

