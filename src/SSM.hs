module SSM where

-- Registers (I added the RR and HP)
data Reg = PC | SP | MP | R3 | R4 | R5 | R6 | R7 | RR | HP
   deriving Show

r0, r1, r2, r3, r4, r5, r6, r7 :: Reg
r0 = PC -- Program Counter
r1 = SP -- Stack Pointer
r2 = MP -- Mark Pointer
r3 = R3
r4 = R4
r5 = R5
r6 = R6
r7 = R7

-- See https://webspace.science.uu.nl/~hage0101/SSM/instructions.html for details
data Instr
    = STR Reg | STL Int  | STS Int  | STA Int        -- Store from stack
    | LDR Reg | LDL Int  | LDS Int  | LDA Int        -- Load on stack
    | LDC Int | LDLA Int | LDSA Int | LDAA Int       -- Load on stack
    | BRA Int | Bra String                           -- Branch always (relative/to label)
    | BRF Int | Brf String                           -- Branch on false
    | BRT Int | Brt String                           -- Branch on true
    | BSR Int | Bsr String                           -- Branch to subroutine
    | ADD | SUB | MUL | DIV | MOD                    -- Arithmetical operations on 2 stack operands
    | EQ  | NE  | LT  | LE  | GT  | GE               -- Relational   operations on 2 stack operands
    | AND | OR  | XOR                                -- Bitwise      operations on 2 stack operands
    | NEG | NOT                                      --              operations on 1 stack operand
    | RET | UNLINK | LINK Int | AJS Int              -- Procedure utilities
    | SWP | SWPR Reg | SWPRR Reg Reg | LDRR Reg Reg  -- Various swaps
    | JSR | TRAP Int | NOP | HALT                    -- Other instructions
    | LABEL String                                   -- Pseudo-instruction for generating a label
    | LDH Int | STH Int                              -- Heap variables
    deriving Show



type Code = [Instr]


----- Utilities -----

pop :: Instr
pop = AJS (-1)

codeSize :: Code -> Int
codeSize = sum . map instrSize

instrSize :: Instr -> Int
instrSize i = case i of {
                  LDRR _ _ -> 3;    SWPRR _ _ -> 3;    BRA  _    -> 2;   BRF  _ -> 2;   BRT  _ -> 2;
                  BSR  _   -> 2;    Bra   _   -> 2;    Brf  _    -> 2;   Brt  _ -> 2;   Bsr  _ -> 2;
                  LDR  _   -> 2;    LDL   _   -> 2;    LDS  _    -> 2;   LDA  _ -> 2;   LDC  _ -> 2;
                  LDLA _   -> 2;    LDSA  _   -> 2;    LDAA _    -> 2;   STR  _ -> 2;   STL  _ -> 2;
                  STS  _   -> 2;    STA   _   -> 2;    AJS  _    -> 2;   LINK _ -> 2;   TRAP _ -> 2;
                  SWPR _   -> 2;    LABEL _   -> 0;    LDH  _    -> 2;   STH  _ -> 2;   otherwise -> 1;
              }

-- For prettyprinting SSM code
formatInstr :: Instr -> String
formatInstr (LABEL s) = s ++ ":"
formatInstr x         = '\t' : show x

formatCode :: Code -> String
formatCode = filter clean . concatMap ((++ "\n") . formatInstr)
    where clean c = c `notElem` "()\""
