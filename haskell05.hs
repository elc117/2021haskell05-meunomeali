-- Prática 04 de Haskell
-- Nome: Álisson Braga Canabarro

-- 1)  Índice de Massa Corporal
bmi :: Float -> Float -> String
bmi peso altura =
  let imc = peso / (altura^2)
   in if imc <= 18.5 then "ABAIXO" else if imc < 30 then "NORMAL" else "ACIMA"

-- 2) IMC usando where
bmi' :: Float -> Float -> String
bmi' peso altura = if imc <= 18.5 then "ABAIXO" else if imc < 30 then "NORMAL" else "ACIMA"
  where imc = peso / (altura^2)

-- 3)

cpfValid :: [Int] -> Bool
cpfValid cpf =
 let digits = take 9 cpf
     dv1 = cpfDV digits [10,9..]
     dv2 = cpfDV (digits ++ [dv1]) [11,10..]
  in dv1 == cpf !! 9 && dv2 == cpf !! 10

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = if expr < 2 then 0 else 11-expr
 where expr = (sum $ zipWith (*) digits mults) `mod` 11
