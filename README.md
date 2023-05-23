# guessNumberLucid
Off-Chain code with Lucid #plutus #cardano #haskell 





Employ Lucid to interact with off-chain code.

Employ blockfrost to connect to Cardano blockchain.





## Plutarch On Chain code

Here we start with Number guess game and the Cardano plutus onChain script is written in Plutarch. 

Below is snippet of onCHain code for :

### Datum and Redeemer

```haskell
data POurDatum (s :: S) = POurDatum (Term s (PDataRecord '[ "password" ':= PInteger ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POurDatum where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData POurDatum 

data POurRedeemer (s :: S) = POurRedeemer (Term s (PDataRecord '[ "password" ':= PInteger ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POurRedeemer where 
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POurRedeemer  
```



### Plutarch validator

This is a simple validator where it matches the existing Datum number with Redeemer number, and if its equal then it unlocks the funds. Again this is a trivial example as we inline the Datum so its actually visible but ideally it should be hashed.

```haskell
pvalidateSmallChecks :: Term s (POurDatum :--> POurRedeemer :--> PScriptContext :--> PUnit)
pvalidateSmallChecks = phoistAcyclic $ plam $ \datum redeemer _ctx -> unTermCont $ do 
    -- ctxF <- pletFieldsC @'["txInfo"] ctx 
    -- infoF <- pletFieldsC @'["signatories"] ctxF.txInfo 
    datumF <- pletFieldsC @'["password"] datum 
    redeemF <- pletFieldsC @'["password"] redeemer 
    pure $
      pif ( (redeemF.password) #== datumF.password )
      (pconstant ())
      perror

pvalidateSmallChecksW :: Term s PValidator 
pvalidateSmallChecksW = phoistAcyclic $ plam $ \datum redeemer ctx ->
    let ourDatum :: Term _ POurDatum 
        ourDatum = punsafeCoerce datum 
        ourRedeemer :: Term _ POurRedeemer 
        ourRedeemer = punsafeCoerce redeemer
     in popaque $ pvalidateSmallChecks # ourDatum # ourRedeemer # ctx 
```





