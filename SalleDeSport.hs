module SalleDeSport
where

-- une formule est créée
-- une formule est supprimée
-- une formule change de prix

type Description = String
type Prix = Double
data Durée = Mois
           | Année
    deriving (Eq, Show)
  
data Formule = Formule Description Prix Durée
    deriving (Eq, Show)

-- un Prospect peut choisir une Formule pour souscrire à un Abonnement
-- Le Prospect Michel, étudiant a souscrit un Abonnement annuel "Tout Benef" 
-- un Abonnement est renouvelé
-- la modification du prix d'une formule ne change pas le prix d'un abonnement

type Nom = String

data Prospect = Prospect Nom StatutEtudiant
    deriving (Eq, Show)

data StatutEtudiant = Etudiant | NonEtudiant
    deriving (Eq, Show)

type NombreRenouvellements = Int

data Abonnement = Abonnement Prix Durée NombreRenouvellements
    deriving (Eq, Show)

data TypeRemise = RemiseEtudiant | RemiseAnnée | RemiseRenouvellement
    deriving (Eq, Show)

remise :: TypeRemise -> Prix -> Prix
remise RemiseEtudiant       = (* (1 - 0.3))
remise RemiseAnnée          = (* (1 - 0.1))
remise RemiseRenouvellement = (* (1 - 0.05))

souscrire :: Prospect -> Formule -> Abonnement
souscrire (Prospect _ statut) (Formule _ prix durée) = 
    promotionEtudiant statut 
        (promotionDuree (Abonnement prix durée 0))  

promotionDuree :: Abonnement -> Abonnement
promotionDuree (Abonnement p Année nbRen) = Abonnement (remise RemiseAnnée p) Année nbRen
promotionDuree a = a 

promotionEtudiant :: StatutEtudiant -> Abonnement -> Abonnement
promotionEtudiant Etudiant (Abonnement p durée nbRen)  = Abonnement (remise RemiseEtudiant p) durée nbRen
promotionEtudiant _ a = a

promotionRenouvellement :: Abonnement -> Abonnement
promotionRenouvellement (Abonnement p durée nbRen) = Abonnement (remise RemiseRenouvellement p)  durée nbRen

renouvellement :: Abonnement -> Abonnement
renouvellement a@(Abonnement _ _ nbRen) | nbRen >= 3  = a
renouvellement (Abonnement prix durée nbRen) = promotionRenouvellement (Abonnement prix durée (succ nbRen))
