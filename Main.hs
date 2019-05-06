import SalleDeSport

statut :: String -> StatutEtudiant
statut s = case lookup s [("O",Etudiant),("o",Etudiant),("N",NonEtudiant),("n",NonEtudiant)] of
    (Just st) -> st
    Nothing -> NonEtudiant
    

main = do
    putStr "Nom:" 
    nom <- getLine
    putStr "Etudiant (O/N):"
    st <- fmap statut getLine
    
    let f = Formule "Un an tout benef" 400.00 Année
    let p = Prospect nom st
    let a = souscrire p f 
    putStrLn (show a)
