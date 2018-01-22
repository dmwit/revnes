module RevNES.Types
	( module RevNES.Types
	, Word8
	, Word16
	) where

import Data.Word

data Size8Operand = Immediate | ZeroPage | ZeroPagePlusX | ZeroPagePlusY | AddXThenDeref | DerefThenAddY
	deriving (Eq, Ord, Read, Show)

data Size16Operand = Absolute | Deref | AbsolutePlusX | AbsolutePlusY
	deriving (Eq, Ord, Read, Show)

data Operand
	= Accumulator
	| Size8  Size8Operand  Word8
	| Size16 Size16Operand Word16
	deriving (Eq, Ord, Read, Show)

data Operator = ADC | AND | ASL | BIT | CMP | CPX | CPY | DEC | EOR | INC | JMP | LDA | LDX | LDY | LSR | ORA | ROL | ROR | SBC | STA | STX | STY
	deriving (Eq, Ord, Read, Show)

data Condition = BCC | BCS | BEQ | BMI | BNE | BPL | BVC | BVS
	deriving (Eq, Ord, Read, Show)

data Implied = BRK | CLC | CLD | CLI | CLV | DEX | DEY | INX | INY | NOP | PHA | PHP | PLA | PLP | RTI | RTS | SEC | SED | SEI | TAX | TAY | TSX | TXA | TXS | TYA
	deriving (Eq, Ord, Read, Show)

data Instruction
	= Op Operator Operand
	| Branch Condition Word8
	| Byte Implied
	| JSR Word16
	deriving (Eq, Ord, Read, Show)

byteLength :: Num a => Instruction -> a
byteLength i = case i of
	Op _ op -> case op of
		Accumulator {} -> 1
		Size8       {} -> 2
		Size16      {} -> 3
	Branch {} -> 2
	Byte   {} -> 1
	JSR    {} -> 3
