# Show be a type error

data Cat:
  | Siamese(age :: Number)
  | SiameseInBothSenses(c1 :: Cat, c2 :: Cat)
end

cat1 = block:
  data Cat:
    | Siamese(age :: Number)
    | SiameseInBothSenses(c1 :: Cat, c2 :: Cat)
  end
  Tabby(1)
end

error = SiameseInBothSenses(cat1, Tabby(7))
