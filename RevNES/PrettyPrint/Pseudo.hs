module RevNES.PrettyPrint.Pseudo where

import Data.Int
import Data.Map (Map)
import RevNES.Types
import Text.Printf
import qualified Data.Map as M

-- TODO: mirroring
data PPEnvironment = PPEnvironment
	{ variables :: Map Word16 String
	, labels    :: Map Word16 String
	} deriving (Eq, Ord, Read, Show)

pp :: PPEnvironment -> Word16 -> Instruction -> String
pp env pc (JSR addr) = ppLabel env addr ++ "()"
pp env pc (Byte b) = case b of
	BRK -> "break"
	CLC -> "flags.carry = 0"
	CLD -> "flags.decimal = 0"
	CLI -> "flags.interrupt = 0"
	CLV -> "flags.overflow = 0"
	DEX -> "x--"
	DEY -> "y--"
	INX -> "x++"
	INY -> "y++"
	NOP -> "nop"
	PHA -> "push(a)"
	PHP -> "push(flags)"
	PLA -> "a = pop()"
	PLP -> "flags = pop()"
	RTI -> "return from interrupt"
	RTS -> "return"
	SEC -> "flags.carry = 1"
	SED -> "flags.decimal = 1"
	SEI -> "flags.interrupt = 1"
	TAX -> "x = a"
	TAY -> "y = a"
	TSX -> "x = s"
	TXA -> "a = x"
	TXS -> "s = x"
	TYA -> "a = y"
pp env pc (Branch cond offset) = "if(" ++ ppCond ++ ") goto " ++ ppLabel env (pc + 1 + signExtend offset) where
	ppCond = case cond of
		BCC -> "flags.carry == 0"
		BCS -> "flags.carry == 1"
		BNE -> "flags.zero == 0"
		BEQ -> "flags.zero == 1"
		BPL -> "flags.negative == 0"
		BMI -> "flags.negative == 1"
		BVC -> "flags.overflow == 0"
		BVS -> "flags.overflow == 1"
pp env pc (Op op target) = ppOp where
	ppOp = case op of
		ADC -> "a += flags.carry + " ++ ppTarget
		AND -> "a &= " ++ ppTarget
		ASL -> ppTarget ++ " *= 2"
		BIT -> "a & " ++ ppTarget
		CMP -> ppTarget ++ " - a"
		CPX -> ppTarget ++ " - x"
		CPY -> ppTarget ++ " - y"
		DEC -> ppTarget ++ " -= 1" -- doesn't use -- because who can remember the operator precedence for *foo--?
		EOR -> "a ^= " ++ ppTarget
		INC -> ppTarget ++ " += 1" -- doesn't use ++ because who can remember the operator precedence for *foo++?
		JMP -> "goto " ++ case target of
			Size16 Absolute addr -> ppLabel env addr
			Size16 Deref    addr -> "*(uint16_t *)" ++ ppVariable env addr
			_ -> "<invalid jump target>"
		LDA -> "a = " ++ ppTarget
		LDX -> "x = " ++ ppTarget
		LDY -> "x = " ++ ppTarget
		LSR -> ppTarget ++ " /= 2"
		ORA -> "a |= " ++ ppTarget
		ROL -> ppTarget ++ " = flags.carry + 2 * " ++ ppTarget
		ROR -> ppTarget ++ " = 0x80*flags.carry + " ++ ppTarget ++ " / 2"
		SBC -> "a -= flags.carry + " ++ ppTarget
		STA -> ppTarget ++ " = a"
		STX -> ppTarget ++ " = x"
		STY -> ppTarget ++ " = y"
	ppTarget = case target of
		Accumulator -> "a"
		Size8 style offset -> case style of
			Immediate     -> printf "0x%02x" offset
			ZeroPage      -> "*" ++ ppOffset
			ZeroPagePlusX -> "*((" ++ ppOffset ++ " + x) & 0x00ff)"
			ZeroPagePlusY -> "*((" ++ ppOffset ++ " + y) & 0x00ff)"
			AddXThenDeref -> "**(uint16_t *)((" ++ ppOffset ++ " + x) & 0x00ff)"
			DerefThenAddY -> "*(*(uint16_t *)" ++ ppOffset ++ " + y)"
			where ppOffset = ppVariable env (fromIntegral offset)
		Size16 style addr -> case style of
			Absolute      -> "*" ++ ppVariable env addr
			Deref         -> "**(uint16_t *)" ++ ppVariable env addr
			AbsolutePlusX -> ppVariable env addr ++ "[x]"
			AbsolutePlusY -> ppVariable env addr ++ "[y]"

signExtend :: Word8 -> Word16
signExtend w8 = fromIntegral (fromIntegral w8 :: Int8)

ppAddress :: Map Word16 String -> Word16 -> String
ppAddress names addr = M.findWithDefault (printf "0x%04x" addr) addr names

ppVariable, ppLabel :: PPEnvironment -> Word16 -> String
ppVariable = ppAddress . variables
ppLabel    = ppAddress . labels
