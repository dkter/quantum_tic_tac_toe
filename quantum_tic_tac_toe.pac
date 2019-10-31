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

symbol
	^symbol!

symbol: aSymbol
	symbol := aSymbol!

turn
	^turn!

turn: anInteger
	turn := anInteger! !
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

at: x and: y
	^(board at: y) at: x!

initialize
	board := Array with: (Array ofSize: 3)
		       with: (Array ofSize: 3)
		       with: (Array ofSize: 3).!

place: tile x: x y: y
	((self at: x and: y) isMemberOf: OrderedCollection) ifTrue: [
		(board at: y) at: x put: tile
	]
	ifFalse: [
		(board at: y) at: x put: (OrderedCollection with: tile)
	]!

placeSuperposition: tile x1: x1 y1: y1 x2: x2 y2: y2
	self place: tile x: x1 y: y1.
	self place: tile x: x2 y: y2.
	! !
!QuantumTTTGame categoriesFor: #at:and:!public! !
!QuantumTTTGame categoriesFor: #initialize!public! !
!QuantumTTTGame categoriesFor: #place:x:y:!private! !
!QuantumTTTGame categoriesFor: #placeSuperposition:x1:y1:x2:y2:!public! !

"Binary Globals"!

