module RevNES.Parse (parseInstruction, parseInstructions, ParseResult(..)) where

import Data.Array
import Data.Attoparsec.ByteString
import Data.Bits
import Data.ByteString (ByteString)
import Numeric
import RevNES.Types
import qualified Data.ByteString as BS

data ParseResult
	= TooShort ByteString
	| UnknownOpcode Word8
	| Success Instruction
	deriving (Eq, Ord, Read, Show)

parseInstructions :: ByteString -> [[Instruction]]
parseInstructions bs = go (BS.length bs) [[]] where
	go 0 rest = init rest
	go n rest = case parseInstruction (BS.drop (n-1) bs) of
		Success i -> go (n-1) ((i:rest !! (byteLength i-1)):rest)
		_ -> go (n-1) ([]:rest)

parseInstruction :: ByteString -> ParseResult
parseInstruction bs = case parse instruction bs of
	Fail    {} -> UnknownOpcode (BS.head bs)
	Partial {} -> TooShort bs
	Done _ i   -> Success i

instruction :: Parser Instruction
instruction = do
	opcode <- anyWord8
	opcodeMap ! opcode

opcodeMap :: Array Word8 (Parser Instruction)
opcodeMap = listArray (minBound, maxBound)
	[ {- 0x00 -} byte BRK
	, {- 0x01 -} addXThenDeref ORA
	, {- 0x02 -} invalid 0x02
	, {- 0x03 -} invalid 0x03
	, {- 0x04 -} invalid 0x04
	, {- 0x05 -} zeroPage ORA
	, {- 0x06 -} zeroPage ASL
	, {- 0x07 -} invalid 0x07
	, {- 0x08 -} byte PHP
	, {- 0x09 -} immediate ORA
	, {- 0x0A -} accumulator ASL
	, {- 0x0B -} invalid 0x0B
	, {- 0x0C -} invalid 0x0C
	, {- 0x0D -} absolute ORA
	, {- 0x0E -} absolute ASL
	, {- 0x0F -} invalid 0x0F
	, {- 0x10 -} branch BPL
	, {- 0x11 -} derefThenAddY ORA
	, {- 0x12 -} invalid 0x12
	, {- 0x13 -} invalid 0x13
	, {- 0x14 -} invalid 0x14
	, {- 0x15 -} zeroPagePlusX ORA
	, {- 0x16 -} zeroPagePlusX ASL
	, {- 0x17 -} invalid 0x17
	, {- 0x18 -} byte CLC
	, {- 0x19 -} absolutePlusY ORA
	, {- 0x1A -} invalid 0x1A
	, {- 0x1B -} invalid 0x1B
	, {- 0x1C -} invalid 0x1C
	, {- 0x1D -} absolutePlusX ORA
	, {- 0x1E -} absolutePlusX ASL
	, {- 0x1F -} invalid 0x1F
	, {- 0x20 -} JSR <$> anyWord16
	, {- 0x21 -} addXThenDeref AND
	, {- 0x22 -} invalid 0x22
	, {- 0x23 -} invalid 0x23
	, {- 0x24 -} zeroPage BIT
	, {- 0x25 -} zeroPage AND
	, {- 0x26 -} zeroPage ROL
	, {- 0x27 -} invalid 0x27
	, {- 0x28 -} byte PLP
	, {- 0x29 -} immediate AND
	, {- 0x2A -} accumulator ROL
	, {- 0x2B -} invalid 0x2B
	, {- 0x2C -} absolute BIT
	, {- 0x2D -} absolute AND
	, {- 0x2E -} absolute ROL
	, {- 0x2F -} invalid 0x2F
	, {- 0x30 -} branch BMI
	, {- 0x31 -} derefThenAddY AND
	, {- 0x32 -} invalid 0x32
	, {- 0x33 -} invalid 0x33
	, {- 0x34 -} invalid 0x34
	, {- 0x35 -} zeroPagePlusX AND
	, {- 0x36 -} zeroPagePlusX ROL
	, {- 0x37 -} invalid 0x37
	, {- 0x38 -} byte SEC
	, {- 0x39 -} absolutePlusY AND
	, {- 0x3A -} invalid 0x3A
	, {- 0x3B -} invalid 0x3B
	, {- 0x3C -} invalid 0x3C
	, {- 0x3D -} absolutePlusX AND
	, {- 0x3E -} absolutePlusX ROL
	, {- 0x3F -} invalid 0x3F
	, {- 0x40 -} byte RTI
	, {- 0x41 -} addXThenDeref EOR
	, {- 0x42 -} invalid 0x42
	, {- 0x43 -} invalid 0x43
	, {- 0x44 -} invalid 0x44
	, {- 0x45 -} zeroPage EOR
	, {- 0x46 -} zeroPage LSR
	, {- 0x47 -} invalid 0x47
	, {- 0x48 -} byte PHA
	, {- 0x49 -} immediate EOR
	, {- 0x4A -} accumulator LSR
	, {- 0x4B -} invalid 0x4B
	, {- 0x4C -} absolute JMP
	, {- 0x4D -} absolute EOR
	, {- 0x4E -} absolute LSR
	, {- 0x4F -} invalid 0x4F
	, {- 0x50 -} branch BVC
	, {- 0x51 -} derefThenAddY EOR
	, {- 0x52 -} invalid 0x52
	, {- 0x53 -} invalid 0x53
	, {- 0x54 -} invalid 0x54
	, {- 0x55 -} zeroPagePlusX EOR
	, {- 0x56 -} zeroPagePlusX LSR
	, {- 0x57 -} invalid 0x57
	, {- 0x58 -} byte CLI
	, {- 0x59 -} absolutePlusY EOR
	, {- 0x5A -} invalid 0x5A
	, {- 0x5B -} invalid 0x5B
	, {- 0x5C -} invalid 0x5C
	, {- 0x5D -} absolutePlusX EOR
	, {- 0x5E -} absolutePlusX LSR
	, {- 0x5F -} invalid 0x5F
	, {- 0x60 -} byte RTS
	, {- 0x61 -} addXThenDeref ADC
	, {- 0x62 -} invalid 0x62
	, {- 0x63 -} invalid 0x63
	, {- 0x64 -} invalid 0x64
	, {- 0x65 -} zeroPage ADC
	, {- 0x66 -} zeroPage ROR
	, {- 0x67 -} invalid 0x67
	, {- 0x68 -} byte PLA
	, {- 0x69 -} immediate ADC
	, {- 0x6A -} accumulator ROR
	, {- 0x6B -} invalid 0x6B
	, {- 0x6C -} deref JMP
	, {- 0x6D -} absolute ADC
	, {- 0x6E -} absolute ROR
	, {- 0x6F -} invalid 0x6F
	, {- 0x70 -} branch BVS
	, {- 0x71 -} derefThenAddY ADC
	, {- 0x72 -} invalid 0x72
	, {- 0x73 -} invalid 0x73
	, {- 0x74 -} invalid 0x74
	, {- 0x75 -} zeroPagePlusX ADC
	, {- 0x76 -} zeroPagePlusX ROR
	, {- 0x77 -} invalid 0x77
	, {- 0x78 -} byte SEI
	, {- 0x79 -} absolutePlusY ADC
	, {- 0x7A -} invalid 0x7A
	, {- 0x7B -} invalid 0x7B
	, {- 0x7C -} invalid 0x7C
	, {- 0x7D -} absolutePlusX ADC
	, {- 0x7E -} absolutePlusX ROR
	, {- 0x7F -} invalid 0x7F
	, {- 0x80 -} invalid 0x80
	, {- 0x81 -} addXThenDeref STA
	, {- 0x82 -} invalid 0x82
	, {- 0x83 -} invalid 0x83
	, {- 0x84 -} zeroPage STY
	, {- 0x85 -} zeroPage STA
	, {- 0x86 -} zeroPage STX
	, {- 0x87 -} invalid 0x87
	, {- 0x88 -} byte DEY
	, {- 0x89 -} invalid 0x89
	, {- 0x8A -} byte TXA
	, {- 0x8B -} invalid 0x8B
	, {- 0x8C -} absolute STY
	, {- 0x8D -} absolute STA
	, {- 0x8E -} absolute STX
	, {- 0x8F -} invalid 0x8F
	, {- 0x90 -} branch BCC
	, {- 0x91 -} derefThenAddY STA
	, {- 0x92 -} invalid 0x92
	, {- 0x93 -} invalid 0x93
	, {- 0x94 -} zeroPagePlusX STY
	, {- 0x95 -} zeroPagePlusX STA
	, {- 0x96 -} zeroPagePlusY STX
	, {- 0x97 -} invalid 0x97
	, {- 0x98 -} byte TYA
	, {- 0x99 -} absolutePlusY STA
	, {- 0x9A -} byte TXS
	, {- 0x9B -} invalid 0x9B
	, {- 0x9C -} invalid 0x9C
	, {- 0x9D -} absolutePlusX STA
	, {- 0x9E -} invalid 0x9E
	, {- 0x9F -} invalid 0x9F
	, {- 0xA0 -} immediate LDY
	, {- 0xA1 -} addXThenDeref LDA
	, {- 0xA2 -} immediate LDX
	, {- 0xA3 -} invalid 0xA3
	, {- 0xA4 -} zeroPage LDY
	, {- 0xA5 -} zeroPage LDA
	, {- 0xA6 -} zeroPage LDX
	, {- 0xA7 -} invalid 0xA7
	, {- 0xA8 -} byte TAY
	, {- 0xA9 -} immediate LDA
	, {- 0xAA -} byte TAX
	, {- 0xAB -} invalid 0xAB
	, {- 0xAC -} absolute LDY
	, {- 0xAD -} absolute LDA
	, {- 0xAE -} absolute LDX
	, {- 0xAF -} invalid 0xAF
	, {- 0xB0 -} branch BCS
	, {- 0xB1 -} derefThenAddY LDA
	, {- 0xB2 -} invalid 0xB2
	, {- 0xB3 -} invalid 0xB3
	, {- 0xB4 -} zeroPagePlusX LDY
	, {- 0xB5 -} zeroPagePlusX LDA
	, {- 0xB6 -} zeroPagePlusY LDX
	, {- 0xB7 -} invalid 0xB7
	, {- 0xB8 -} byte CLV
	, {- 0xB9 -} absolutePlusY LDA
	, {- 0xBA -} byte TSX
	, {- 0xBB -} invalid 0xBB
	, {- 0xBC -} absolutePlusX LDY
	, {- 0xBD -} absolutePlusX LDA
	, {- 0xBE -} absolutePlusY LDX
	, {- 0xBF -} invalid 0xBF
	, {- 0xC0 -} immediate CPY
	, {- 0xC1 -} addXThenDeref CMP
	, {- 0xC2 -} invalid 0xC2
	, {- 0xC3 -} invalid 0xC3
	, {- 0xC4 -} zeroPage CPY
	, {- 0xC5 -} zeroPage CMP
	, {- 0xC6 -} zeroPage DEC
	, {- 0xC7 -} invalid 0xC7
	, {- 0xC8 -} byte INY
	, {- 0xC9 -} immediate CMP
	, {- 0xCA -} byte DEX
	, {- 0xCB -} invalid 0xCB
	, {- 0xCC -} absolute CPY
	, {- 0xCD -} absolute CMP
	, {- 0xCE -} absolute DEC
	, {- 0xCF -} invalid 0xCF
	, {- 0xD0 -} branch BNE
	, {- 0xD1 -} derefThenAddY CMP
	, {- 0xD2 -} invalid 0xD2
	, {- 0xD3 -} invalid 0xD3
	, {- 0xD4 -} invalid 0xD4
	, {- 0xD5 -} zeroPagePlusX CMP
	, {- 0xD6 -} zeroPagePlusX DEC
	, {- 0xD7 -} invalid 0xD7
	, {- 0xD8 -} byte CLD
	, {- 0xD9 -} absolutePlusY CMP
	, {- 0xDA -} invalid 0xDA
	, {- 0xDB -} invalid 0xDB
	, {- 0xDC -} invalid 0xDC
	, {- 0xDD -} absolutePlusX CMP
	, {- 0xDE -} absolutePlusX DEC
	, {- 0xDF -} invalid 0xDF
	, {- 0xE0 -} immediate CPX
	, {- 0xE1 -} addXThenDeref SBC
	, {- 0xE2 -} invalid 0xE2
	, {- 0xE3 -} invalid 0xE3
	, {- 0xE4 -} zeroPage CPX
	, {- 0xE5 -} zeroPage SBC
	, {- 0xE6 -} zeroPage INC
	, {- 0xE7 -} invalid 0xE7
	, {- 0xE8 -} byte INX
	, {- 0xE9 -} immediate SBC
	, {- 0xEA -} byte NOP
	, {- 0xEB -} invalid 0xEB
	, {- 0xEC -} absolute CPX
	, {- 0xED -} absolute SBC
	, {- 0xEE -} absolute INC
	, {- 0xEF -} invalid 0xEF
	, {- 0xF0 -} branch BEQ
	, {- 0xF1 -} derefThenAddY SBC
	, {- 0xF2 -} invalid 0xF2
	, {- 0xF3 -} invalid 0xF3
	, {- 0xF4 -} invalid 0xF4
	, {- 0xF5 -} zeroPagePlusX SBC
	, {- 0xF6 -} zeroPagePlusX INC
	, {- 0xF7 -} invalid 0xF7
	, {- 0xF8 -} byte SED
	, {- 0xF9 -} absolutePlusY SBC
	, {- 0xFA -} invalid 0xFA
	, {- 0xFB -} invalid 0xFB
	, {- 0xFC -} invalid 0xFC
	, {- 0xFD -} absolutePlusX SBC
	, {- 0xFE -} absolutePlusX INC
	, {- 0xFF -} invalid 0xFF
	]

invalid :: Word8 -> Parser Instruction
invalid w = fail $ "invalid opcode 0x" ++ showHex w ""

accumulator, immediate, zeroPage, zeroPagePlusX, zeroPagePlusY, absolute, deref, absolutePlusX, absolutePlusY, addXThenDeref, derefThenAddY :: Operator -> Parser Instruction
accumulator op = return (Op op Accumulator)
[immediate, zeroPage, zeroPagePlusX, zeroPagePlusY, addXThenDeref, derefThenAddY] =
	[ \op -> Op op . Size8 con <$> anyWord8
	| con <- [Immediate, ZeroPage, ZeroPagePlusX, ZeroPagePlusY, AddXThenDeref, DerefThenAddY]
	]
[absolute, deref, absolutePlusX, absolutePlusY] =
	[ \op -> Op op . Size16 con <$> anyWord16
	| con <- [Absolute, Deref, AbsolutePlusX, AbsolutePlusY]
	]

byte :: Implied -> Parser Instruction
byte = return . Byte

branch :: Condition -> Parser Instruction
branch cond = Branch cond <$> anyWord8

anyWord16 :: Parser Word16
anyWord16 = do
	lo <- anyWord8
	hi <- anyWord8
	return ((fromIntegral hi `shiftL` 8) .|. fromIntegral lo)
