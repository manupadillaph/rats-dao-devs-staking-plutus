{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE BangPatterns #-}
{- HLINT ignore "Use camelCase" -}


module Test1 where

import qualified Data.ByteString.Short                         as DataByteStringShort
import qualified Data.ByteString.Lazy                          as DataByteStringLazy
import qualified Codec.Serialise                               as CodecSerialise
import qualified Data.Maybe                                    as DataMaybe
import qualified Ledger
import qualified Ledger.Ada                                    as LedgerAda
import qualified Ledger.Address                                as LedgerAddress
import qualified Ledger.Value                                  as LedgerValue
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Scripts                as UtilsScriptsV2
import qualified Plutus.V1.Ledger.ProtocolVersions             as LedgerProtocolVersionsV1  
import qualified Plutus.V1.Ledger.Scripts                      as LedgerScriptsV1
import qualified Plutus.V2.Ledger.Api                          as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts                     as LedgerContextsV2
import qualified Plutus.V2.Ledger.EvaluationContext            as LedgerEvaluationContextV2
import qualified PlutusTx
import qualified PlutusTx.AssocMap                             as TxAssocMap
import           PlutusTx.Prelude
import qualified Prelude                                       as P
import qualified PlutusTx.Builtins                             as TxBuiltins

import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers
import qualified Validators.StakePlusV2.Helpers                             as Helpers 
import qualified Validators.StakePlusV2.Types.Examples         as T  
import qualified Validators.StakePlusV2.Types.DatumsValidator       as T (DatumValidator, PoolDatumTypo (..), TxOut_With_Datum, DatumValidator( PoolDatum ) )
---------------------------------------------------

{-# INLINABLE sortFlattenValueWithoutZeros #-}
sortFlattenValueWithoutZeros :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
sortFlattenValueWithoutZeros  (LedgerValue.Value !mp) =
    let
        sort_CurrencySymbol :: (LedgerApiV2.CurrencySymbol, a) -> (LedgerApiV2.CurrencySymbol, a) -> Ordering
        sort_CurrencySymbol (!cs1, _) (!cs2, _) = compare cs1 cs2
        sort_TokenName :: (LedgerApiV2.TokenName, a) -> (LedgerApiV2.TokenName, a) -> Ordering
        sort_TokenName (!tn1, _) (!tn2, _) = compare tn1 tn2
        !f1 = sortBy sort_CurrencySymbol (TxAssocMap.toList mp)
        !f2 = [ ( cs ,  sortBy sort_TokenName  (TxAssocMap.toList mp')) | (cs, mp') <- f1 ]
        !f3 = [ (cs , tn, amt) | (cs, f4) <- f2, (tn, amt) <- f4, amt /= 0 ]
    in
        f3

{-# INLINABLE flattenValueWithoutZeros #-}
flattenValueWithoutZeros :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
flattenValueWithoutZeros (LedgerValue.Value !mp) =
    let
        !f1 = TxAssocMap.toList mp
        !f2 = [ ( cs , TxAssocMap.toList mp') | (cs, mp') <- f1 ]
        !f3 = [ (cs , tn, amt) | (cs, f4) <- f2, (tn, amt) <- f4, amt /= 0 ]
    in
        f3

{-# INLINABLE sortFlattenValueList #-}
sortFlattenValueList :: [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
sortFlattenValueList !xs =
    let
        sort_CurrencySymbolAndTokenName :: (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, a) -> (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, a) -> Ordering
        sort_CurrencySymbolAndTokenName (!cs1, !tn1, _) (!cs2, !tn2, _) = 
            case compare cs1 cs2 of
                EQ -> compare tn1 tn2
                x -> x
        !f1 = sortBy sort_CurrencySymbolAndTokenName xs
    in
        f1

{-# INLINABLE normalizeValue #-}
normalizeValue :: LedgerApiV2.Value -> LedgerApiV2.Value
normalizeValue = LedgerApiV2.Value . TxAssocMap.fromList . sort . filterRange (/=TxAssocMap.empty)
               . mapRange normalizeTokenMap . TxAssocMap.toList . LedgerApiV2.getValue
  where normalizeTokenMap = TxAssocMap.fromList . sort . filterRange (/=0) . TxAssocMap.toList
        filterRange p kvs = [(k,v) | (k,v) <- kvs, p v]
        mapRange f xys = [(x,f y) | (x,y) <- xys]
        sort xs = sortBy compare xs

---------------------------------------------------

{-# INLINABLE valueEqualsValue #-}
-- sirve cuando hay menos de 15 assets
valueEqualsValue :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue !value1 !value2 =
    let
        !value1' = flattenValueWithoutZeros value1
        !value2' = flattenValueWithoutZeros value2
    in
        length value1' == length value2' &&
        all (\(cs, tn, amount) ->
            let
                !ac = LedgerValue.AssetClass (cs, tn)
            in
                LedgerValue.assetClassValueOf value2 ac == amount
        ) value1'

---------------------------------------------------

{-# INLINABLE valueEqualsValue4 #-}
valueEqualsValue4 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue4 !value1 !value2 =
    let
        !value1' = flattenValueWithoutZeros value1
        !value2' = flattenValueWithoutZeros value2
        
        compareValues :: [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> Bool
        compareValues [] [] = True
        compareValues ((cs1, tn1, amt1):xs1) ((cs2, tn2, amt2):xs2) =
            if cs1 == cs2 && tn1 == tn2 && amt1 == amt2 then
                compareValues xs1 xs2
            else
                compareValues2 (cs1, tn1, amt1) xs1 [(cs2, tn2, amt2)] xs2
        compareValues _ _ = False

        compareValues2 :: (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer) -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> Bool
        compareValues2 (cs1, tn1, amt1) xs1 xs2 ((cs2, tn2, amt2):xs3) =
            if cs1 == cs2 && tn1 == tn2 && amt1 == amt2 then
                compareValues xs1 (xs2++xs3)
            else
                compareValues2 (cs1, tn1, amt1) xs1 ((cs2, tn2, amt2):xs2) xs3
        compareValues2 _ _ _ _ = False
    
    in
        compareValues value1' value2' 

---------------------------------------------------

{-# INLINABLE valueEqualsValue2 #-}
valueEqualsValue2 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue2 !value1 !value2 =
    let
        !value1' = sortFlattenValueWithoutZeros value1
        !value2' = sortFlattenValueWithoutZeros value2
    in
        TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData value1') == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData value2')

---------------------------------------------------

{-# INLINABLE valueEqualsValue3 #-}
-- poco eficiente
valueEqualsValue3 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue3 !value1 !value2 =
    let
        !value1' = normalizeValue value1
        !value2' = normalizeValue value2
    in
        TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData value1') == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData value2')

---------------------------------------------------

{-# INLINABLE valueEqualsValue5 #-}
valueEqualsValue5 :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue5 !value1 !value2 =
    let
        !value1' = flattenValueWithoutZeros value1
    in 
        if length value1' > 15 then
            let
                !value1'' = sortFlattenValueList value1'
                !value2' = sortFlattenValueWithoutZeros value2
            in
                TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData value1'') == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData value2')
        else
            let
                !value2' = flattenValueWithoutZeros value2
            in
                length value1' == length value2' &&
                all (\(cs, tn, amount) ->
                        let
                            !ac = LedgerValue.AssetClass (cs, tn)
                        in
                            LedgerValue.assetClassValueOf value2 ac == amount
                    ) value1'
            
---------------------------------------------------

{-# INLINABLE unsafeValueEqualsValue #-}
-- tienen que estar normalizados, o sea, ordenados y sin ceros
unsafeValueEqualsValue :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
unsafeValueEqualsValue !value1 !value2 =
  TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData value1) == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData value2)


{-# INLINABLE unsafeDatumEqualsDatum #-}
-- tienen que estar normalizados, o sea, mismo orden y mismos campos
unsafeDatumEqualsDatum :: (PlutusTx.ToData d) => d -> d -> Bool
unsafeDatumEqualsDatum !dat1 !dat2 =
  TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData dat1) == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData dat2)


---------------------------------------------------

{-# INLINABLE valuesAndDatumsEqualsValuesAndDatums #-}
valuesAndDatumsEqualsValuesAndDatums :: LedgerApiV2.ToData d => [(LedgerApiV2.Value, d)] -> [(LedgerApiV2.Value, d)] -> Bool
valuesAndDatumsEqualsValuesAndDatums !valuesAndDatums1 !valuesAndDatums2 =
    let
        !flattenValuesAndDatums1 = [(Helpers.flattenValueWithoutZeros v, d) | (v, d) <- valuesAndDatums1]
        !flattenValuesAndDatums2 = [(Helpers.flattenValueWithoutZeros v, d) | (v, d) <- valuesAndDatums2]

        flattenValuesAndDatumsEqualsFlattenValuesAndDatums :: LedgerApiV2.ToData d => [([(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)], d)] -> [([(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)], d)] -> Bool
        flattenValuesAndDatumsEqualsFlattenValuesAndDatums [] [] = True
        flattenValuesAndDatumsEqualsFlattenValuesAndDatums ((flattenValue1, d1):xs1) ((flattenValue2, d2):xs2) =
            if flattenValue1 `Helpers.unsafeFlattenValueEqualsFlattenValue` flattenValue2 && d1 `Helpers.unsafeDatumEqualsDatum` d2 then
                flattenValuesAndDatumsEqualsFlattenValuesAndDatums xs1 xs2
            else
                flattenValuesAndDatumsEqualsFlattenValuesAndDatums2 (flattenValue1, d1) xs1 [(flattenValue2, d2)] xs2
        flattenValuesAndDatumsEqualsFlattenValuesAndDatums _ _ = False

        flattenValuesAndDatumsEqualsFlattenValuesAndDatums2 :: LedgerApiV2.ToData d => ([(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)], d) -> [([(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)], d)] -> [([(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)], d)] -> [([(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)], d)] -> Bool
        flattenValuesAndDatumsEqualsFlattenValuesAndDatums2 (flattenValue1, d1) xs1 xs2 ((flattenValue2, d2):xs3) =
            if flattenValue1 `Helpers.unsafeFlattenValueEqualsFlattenValue` flattenValue2 && d1 `Helpers.unsafeDatumEqualsDatum` d2 then
                flattenValuesAndDatumsEqualsFlattenValuesAndDatums xs1 (xs2++xs3)
            else
                flattenValuesAndDatumsEqualsFlattenValuesAndDatums2 (flattenValue1, d1) xs1 ((flattenValue2, d2):xs2) xs3
        flattenValuesAndDatumsEqualsFlattenValuesAndDatums2 _ _ _ _ = False
    
    in
        flattenValuesAndDatums1 `flattenValuesAndDatumsEqualsFlattenValuesAndDatums` flattenValuesAndDatums2

---------------------------------------------------

{-# INLINABLE valuesAndDatumsEqualsValuesAndDatums2 #-}
valuesAndDatumsEqualsValuesAndDatums2 :: LedgerApiV2.ToData d => [(LedgerApiV2.Value, d)] -> [(LedgerApiV2.Value, d)] -> Bool
valuesAndDatumsEqualsValuesAndDatums2 !valuesAndDatums1 !valuesAndDatums2 =    
    length valuesAndDatums1 == length valuesAndDatums2
    &&
    all (\(v, d) ->
        any (\(v', d') ->
                d `Helpers.unsafeDatumEqualsDatum` d' && v `Helpers.valueEqualsValue` v'
            ) valuesAndDatums2
    ) valuesAndDatums1

---------------------------------------------------

{-# INLINABLE valuesAndDatumsEqualsValuesAndDatums3 #-}
valuesAndDatumsEqualsValuesAndDatums3 :: LedgerApiV2.ToData d => [(LedgerApiV2.Value, d)] -> [(LedgerApiV2.Value, d)] -> Bool
valuesAndDatumsEqualsValuesAndDatums3 !valuesAndDatums1 !valuesAndDatums2 =    
    length valuesAndDatums1 == length valuesAndDatums2
    &&
    all (
        \(v, d) -> 
            isJust (find (\(v', d') ->  d `Helpers.unsafeDatumEqualsDatum` d' && v `Helpers.valueEqualsValue` v' ) valuesAndDatums2)
    ) valuesAndDatums1

---------------------------------------------------

{-# INLINABLE valuesAndDatumsEqualsValuesAndDatums4 #-}
valuesAndDatumsEqualsValuesAndDatums4 :: LedgerApiV2.ToData d => [(LedgerApiV2.Value, d)] -> [(LedgerApiV2.Value, d)] -> Bool
valuesAndDatumsEqualsValuesAndDatums4 !valuesAndDatums1 !valuesAndDatums2 =    
    TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData valuesAndDatums1) == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData valuesAndDatums2)

---------------------------------------------------


{-# INLINABLE mkPolicy1 #-}
mkPolicy1 :: BuiltinData -> BuiltinData -> ()
mkPolicy1 !mintRedeemerRaw !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_WithDatum = OnChainHelpers.getInputsWithDatum ctx
        !outputs_WithDatum = OnChainHelpers.getOutputsWithDatum ctx
        !inputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum ]
        !outputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- outputs_WithDatum ]

        -- !poolDatum_IN = Helpers.getPoolDatumTypo_FromDatum (snd $ inputs_WithDatum!!0)
        -- !poolDatum_OUT = Helpers.getPoolDatumTypo_FromDatum (snd $ outputs_WithDatum!!0)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]
        

        !value1 = inputs_Values!!0
        !value2 = LedgerAda.lovelaceValueOf 500
        !value3_Control = value2 <> value1
        !value3_Real = outputs_Values!!0

        !value4_Real = outputs_Values!!1

        !value_Tk1     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "4aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff33")) 1
        !value_Tk2     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "5aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff44")) 100
        !value_ADA = LedgerAda.lovelaceValueOf 500
        -- !value_ADA1 = LedgerAda.lovelaceValueOf 200
        -- !value_ADA2 = LedgerAda.lovelaceValueOf 300
        !value_For_Control1 =  value_Tk1 <> value_Tk2  <> value_ADA
        !value_For_Control2 =  value_ADA <> value_Tk1  <> value_Tk2
    in
       if 
            -- traceIfFalse "error1" (value3_Control `valueEqualsValue` value3_Real) &&
            -- traceIfFalse "error2" (value_For_Control1 `valueEqualsValue` value4_Real) &&
            -- traceIfFalse "error3" (value_For_Control2 `valueEqualsValue` value4_Real) -- &&
            -- traceIfFalse "datum1" (poolDatum_IN == poolDatum_OUT)
            traceIfFalse "datum1" (inputs_TxOut_Values_And_Datums `valuesAndDatumsEqualsValuesAndDatums` outputs_TxOut_Values_And_Datums)

        then ()
        else error ()

{-# INLINABLE mkPolicy2 #-}
mkPolicy2 :: BuiltinData -> BuiltinData -> ()
mkPolicy2 !mintRedeemerRaw !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_WithDatum = OnChainHelpers.getInputsWithDatum ctx
        !outputs_WithDatum = OnChainHelpers.getOutputsWithDatum ctx
        !inputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum ]
        !outputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- outputs_WithDatum ]

        -- !poolDatum_IN = Helpers.getPoolDatumTypo_FromDatum (snd $ inputs_WithDatum!!0)
        -- !poolDatum_OUT = Helpers.getPoolDatumTypo_FromDatum (snd $ outputs_WithDatum!!0)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]
        
        !value1 = inputs_Values!!0
        !value2 = LedgerAda.lovelaceValueOf 500
        !value3_Control = value2 <> value1
        !value3_Real = outputs_Values!!0

        !value4_Real = outputs_Values!!1

        !value_Tk1     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "4aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff33")) 1
        !value_Tk2     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "5aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff44")) 100
        !value_ADA = LedgerAda.lovelaceValueOf 500
        !value_For_Control1 =  value_Tk1 <> value_Tk2  <> value_ADA
        !value_For_Control2 =  value_ADA <> value_Tk1  <> value_Tk2
    in
        if 
            -- traceIfFalse "error1" (value3_Control `valueEqualsValue1` value3_Real) &&
            -- traceIfFalse "error2" (value_For_Control1 `valueEqualsValue1` value4_Real) &&
            -- traceIfFalse "error3" (value_For_Control2 `valueEqualsValue1` value4_Real)
            traceIfFalse "datum1" (inputs_TxOut_Values_And_Datums `valuesAndDatumsEqualsValuesAndDatums2` outputs_TxOut_Values_And_Datums)

        then ()
        else error ()


{-# INLINABLE mkPolicy3 #-}
mkPolicy3 :: BuiltinData -> BuiltinData -> ()
mkPolicy3 !mintRedeemerRaw !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw

        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)

        !inputs_WithDatum = OnChainHelpers.getInputsWithDatum ctx
        !outputs_WithDatum = OnChainHelpers.getOutputsWithDatum ctx

        !inputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum ]
        !outputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- outputs_WithDatum ]

        -- !sw = OnChainHelpers.tracetxOuts (fst <$> inputs_WithDatum) ctx
        -- !sw2 = OnChainHelpers.tracetxOuts (fst <$> outputs_WithDatum) ctx

        -- !poolDatum_IN = Helpers.getPoolDatumTypo_FromDatum (snd $ inputs_WithDatum!!0)
        -- !poolDatum_OUT = Helpers.getPoolDatumTypo_FromDatum (snd $ outputs_WithDatum!!0)

        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value1 = inputs_Values!!0
        !value2 = LedgerAda.lovelaceValueOf 500
        !value3_Control = value2 <> value1
        !value3_Real = outputs_Values!!0

        !value4_Real = outputs_Values!!1

        !value_Tk1     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "4aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff33")) 1
        !value_Tk2     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "5aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff44")) 100
        !value_ADA = LedgerAda.lovelaceValueOf 500
        !value_For_Control1 =  value_Tk1 <> value_Tk2  <> value_ADA
        !value_For_Control2 =  value_ADA <> value_Tk1  <> value_Tk2

    in
        if 
            -- traceIfFalse "error1" (value3_Control `valueEqualsValue2` value3_Real) &&
            -- traceIfFalse "error2" (value_For_Control1 `valueEqualsValue2` value4_Real) &&
            -- traceIfFalse "error3" (value_For_Control2 `valueEqualsValue2` value4_Real) 
            traceIfFalse "datum1" (inputs_TxOut_Values_And_Datums `valuesAndDatumsEqualsValuesAndDatums3` outputs_TxOut_Values_And_Datums)

        then ()
        else error ()

---------------------------------------------------

{-# INLINEABLE policy1 #-}
policy1 :: LedgerApiV2.MintingPolicy
policy1 = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy1 ||])

{-# INLINEABLE policy2 #-}
policy2 :: LedgerApiV2.MintingPolicy
policy2 = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy2 ||])

{-# INLINEABLE policy3 #-}
policy3 :: LedgerApiV2.MintingPolicy
policy3 = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy3 ||])

---------------------------------------------------

{-# INLINEABLE policy1_fromPlutonomy #-}
policy1_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy1_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy1 ||])

{-# INLINEABLE policy2_fromPlutonomy #-}
policy2_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy2_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy2 ||])

{-# INLINEABLE policy3_fromPlutonomy #-}
policy3_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy3_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy3 ||])

---------------------------------------------------

curSymbol :: LedgerApiV2.MintingPolicy -> LedgerApiV2.CurrencySymbol
curSymbol = UtilsScriptsV2.scriptCurrencySymbol

---------------------------------------------------

evaluate :: P.IO ()
evaluate =
    let

        !value_Tk1     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff1")) 1
        !value_Tk2     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff2")) 1
        !value_Tk3     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff3")) 1
        !value_Tk4     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff4")) 1
        !value_Tk5     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "33a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff5")) 1
        !value_Tk6     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "33a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff6")) 1
        !value_Tk7     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "33a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff7")) 1
        !value_Tk8     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "33a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff8")) 1
        !value_Tk9     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "33a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff9")) 1
        !value_Tk11    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "33a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "ffdd11")) 1
        !value_Tk21    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "33a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "ffdd21")) 1
        !value_Tk31    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "33a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "ffdd31")) 1
        !value_Tk41    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "ffdd41")) 1
        !value_Tk51    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "44a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "ffdd51")) 1
        !value_Tk61    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "44a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "ffdd61")) 1
        !value_Tk71    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "44a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "ffdd71")) 1
        !value_Tk81    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "44a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "ffdd81")) 1
        !value_Tk91    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "44a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "ffdd91")) 1
        !value_Tk111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "44a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee111")) 1
        !value_Tk211   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "44a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee211")) 1
        !value_Tk311   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "44a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee311")) 1
        !value_Tk411   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "55a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee411")) 1
        !value_Tk511   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "55a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee511")) 1
        !value_Tk611   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "55a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee611")) 1
        !value_Tk711   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "55a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee711")) 1
        !value_Tk811   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "55a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee811")) 1
        !value_Tk911   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "55a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee911")) 1
        
        !value_Tk1111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "66a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee1111")) 1
        !value_Tk2111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "66a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee2111")) 1
        !value_Tk3111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "66a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee3111")) 1
        !value_Tk4111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "66a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee4111")) 1
        !value_Tk5111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "66a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee5111")) 1
        !value_Tk6111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "66a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee6111")) 1
        !value_Tk7111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "66a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee7111")) 1
        !value_Tk8111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "66a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee8111")) 1
        !value_Tk9111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "66a8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaddee9111")) 1
        
        !testValuesList = [value_Tk1, value_Tk2, value_Tk3, value_Tk4, value_Tk5, value_Tk6, value_Tk7, value_Tk8, value_Tk9, value_Tk11, value_Tk21, value_Tk31, value_Tk41, value_Tk51, value_Tk61, value_Tk71, value_Tk81, value_Tk91, value_Tk111, value_Tk211, value_Tk311, value_Tk411, value_Tk511, value_Tk611, value_Tk711, value_Tk811, value_Tk911, value_Tk1111, value_Tk2111, value_Tk3111, value_Tk4111, value_Tk5111, value_Tk6111, value_Tk7111, value_Tk8111, value_Tk9111]

        !testValues =  [ foldl (<>) (LedgerAda.lovelaceValueOf 0) (P.take i testValuesList) | i <- [0..36] ]
       
        testCases :: LedgerValue.Value -> P.IO ()
        testCases = do
            evaluateCase

    in
        mapM_ testCases testValues

---------------------------------------------------

evaluateScriptMint :: LedgerApiV2.MintingPolicy -> [PlutusTx.Data] -> (P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, LedgerApiV2.LogOutput, Integer)
evaluateScriptMint p datas =
    let
        getScriptMintingPolicy :: LedgerApiV2.MintingPolicy -> LedgerApiV2.Script
        getScriptMintingPolicy    = LedgerApiV2.getMintingPolicy

        getScriptShortBs :: LedgerApiV2.Script -> DataByteStringShort.ShortByteString
        getScriptShortBs = DataByteStringShort.toShort . DataByteStringLazy.toStrict . CodecSerialise.serialise

        exBudget :: LedgerApiV2.ExBudget
        exBudget = LedgerApiV2.ExBudget 10000000000 14000000

        !pv = LedgerProtocolVersionsV1.vasilPV 
        !scriptMintingPolicyV2 = getScriptMintingPolicy p
        !scriptShortBsV2 = getScriptShortBs scriptMintingPolicyV2
        !(log, e) = LedgerApiV2.evaluateScriptRestricting pv LedgerApiV2.Verbose LedgerEvaluationContextV2.evalCtxForTesting exBudget scriptShortBsV2 datas
        !size = LedgerScriptsV1.scriptSize scriptMintingPolicyV2
   in
        (e, log, size)

---------------------------------------------------

evaluateCase :: LedgerValue.Value -> P.IO ()
evaluateCase v = 
    let
        !curSymbol1 = curSymbol policy1
        !curSymbol2 = curSymbol policy2
        !curSymbol3 = curSymbol policy3

        exampleTxOutRef :: LedgerApiV2.TxOutRef
        exampleTxOutRef = LedgerApiV2.TxOutRef {
            LedgerApiV2.txOutRefId = "aaccff",
            LedgerApiV2.txOutRefIdx = 10
        }

        exampleAddress :: LedgerAddress.Address
        exampleAddress =  LedgerAddress.Address {LedgerApiV2.addressCredential =  LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash  "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e" , LedgerApiV2.addressStakingCredential =  Nothing }

        caseValue = length $ LedgerValue.flattenValue v

        !value_Tk1     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "4aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff33")) 1
        !value_Tk2     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "5aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759", LedgerApiV2.TokenName "aaff44")) 100
        !value_ADA = LedgerAda.lovelaceValueOf 500

        value1 = v <> value_Tk1
        value2 = LedgerAda.lovelaceValueOf 500
        value3 = value_Tk1 <> value_Tk2  <> value_ADA

        value1' = value_Tk1 <> v
        value2' = LedgerAda.lovelaceValueOf 500 
        value3' = value_ADA <> value_Tk2 <> value_Tk1 
        
        -- !value4 =  value_Tk1 <> value_Tk2  <> value_ADA

        !pool1 = T.examplePoolDatum
        !(T.PoolDatum pool1Typo) = pool1
        !pool2 = T.PoolDatum $ Helpers.mkUpdated_PoolDatum_With_Terminated pool1Typo 
        !pool3 = T.PoolDatum $ Helpers.mkUpdated_PoolDatum_With_ClosedAt pool1Typo 0 
 
        mockInput1 :: LedgerApiV2.TxInInfo
        mockInput1 =
            LedgerApiV2.TxInInfo
                exampleTxOutRef
                (LedgerApiV2.TxOut
                    exampleAddress -- txOutAddress :: Address	 
                    value1 -- txOutValue :: Value	 
                    --(LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ()) -- txOutDatum :: OutputDatum	
                    (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData pool1)
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockInput2 :: LedgerApiV2.TxInInfo
        mockInput2 =
            LedgerApiV2.TxInInfo
                exampleTxOutRef
                (LedgerApiV2.TxOut
                    exampleAddress -- txOutAddress :: Address	 
                    value2 -- txOutValue :: Value	 
                    -- (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ()) -- txOutDatum :: OutputDatum	
                    (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData pool2)
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )        

        mockInput3 :: LedgerApiV2.TxInInfo
        mockInput3 =
            LedgerApiV2.TxInInfo
                exampleTxOutRef
                (LedgerApiV2.TxOut
                    exampleAddress -- txOutAddress :: Address	 
                    value3 -- txOutValue :: Value	 
                    -- (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ()) -- txOutDatum :: OutputDatum	
                    (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData pool3)
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )    

        mockOutput1 :: LedgerApiV2.TxOut
        mockOutput1 =
            LedgerApiV2.TxOut
                exampleAddress -- txOutAddress :: Address	 
                value1' -- txOutValue :: Value	 
                --(LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ()) -- txOutDatum :: OutputDatum
                (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData pool1)	
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                
        mockOutput2 :: LedgerApiV2.TxOut
        mockOutput2 =
            LedgerApiV2.TxOut
                exampleAddress -- txOutAddress :: Address	 
                value2' -- txOutValue :: Value	 
                -- (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ()) -- txOutDatum :: OutputDatum
                (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData pool2)
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        mockOutput3 :: LedgerApiV2.TxOut
        mockOutput3 =
            LedgerApiV2.TxOut
                exampleAddress -- txOutAddress :: Address	 
                value3' -- txOutValue :: Value	 
                -- (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ()) -- txOutDatum :: OutputDatum
                (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData pool3)
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        --------------------------------

        redeemer_For_Mint = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData ()

        mockTxInfoInputs :: [LedgerApiV2.TxInInfo]
        mockTxInfoInputs = [ mockInput2, mockInput1, mockInput3 ]

        mockTxInfoOutputs :: [LedgerApiV2.TxOut]
        mockTxInfoOutputs = [ mockOutput3, mockOutput2, mockOutput1 ]

        mockTxInfoMint :: LedgerValue.Value
        mockTxInfoMint = LedgerAda.lovelaceValueOf 0

        mockScriptPurposeMint1 :: LedgerApiV2.ScriptPurpose    
        mockScriptPurposeMint1 = LedgerApiV2.Minting curSymbol1

        mockScriptPurposeMint2 :: LedgerApiV2.ScriptPurpose    
        mockScriptPurposeMint2 = LedgerApiV2.Minting curSymbol2

        mockScriptPurposeMint3 :: LedgerApiV2.ScriptPurpose    
        mockScriptPurposeMint3 = LedgerApiV2.Minting curSymbol3

        mockRedeemerMint1 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint1 = (mockScriptPurposeMint1, redeemer_For_Mint)

        mockRedeemerMint2 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint2 = (mockScriptPurposeMint2, redeemer_For_Mint)

        mockRedeemerMint3 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint3 = (mockScriptPurposeMint3, redeemer_For_Mint)

        mockTxInfoRedeemers1 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers1 = LedgerApiV2.fromList
            [
                mockRedeemerMint1
            ]

        mockTxInfoRedeemers2 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers2 = LedgerApiV2.fromList
            [
                mockRedeemerMint2
            ]

        mockTxInfoRedeemers3 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers3 = LedgerApiV2.fromList
            [
                mockRedeemerMint3
            ]

        !now = LedgerApiV2.POSIXTime 1000000
        !intervalOffset1 = 1000
        !intervalOffset2 = 15000
        !validityRange   = Ledger.interval ( now - intervalOffset1 ) (now + intervalOffset2)

        mockCtx1 :: LedgerApiV2.ScriptContext
        mockCtx1 =
            LedgerApiV2.ScriptContext
                (
                LedgerApiV2.TxInfo
                    mockTxInfoInputs -- txInfoInputs :: [TxInInfo]	
                    [ ] -- txInfoReferenceInputs :: [TxInInfo]
                    mockTxInfoOutputs -- txInfoOutputs :: [TxOut]	
                    (LedgerAda.lovelaceValueOf 50000) -- txInfoFee :: Value	
                    mockTxInfoMint -- txInfoMint :: Value	
                    [] -- txInfoDCert :: [DCert]	
                    (LedgerApiV2.fromList []) -- txInfoWdrl :: Map StakingCredential Integer	
                    validityRange -- txInfoValidRange :: POSIXTimeRange	
                    []  -- txInfoSignatories :: [PubKeyHash]	
                    mockTxInfoRedeemers1 -- txInfoRedeemers :: Map ScriptPurpose Redeemer	
                    (LedgerApiV2.fromList [])  -- txInfoData :: Map DatumHash Datum	
                    (LedgerApiV2.TxId "555") -- txInfoId :: TxId 
                )
                mockScriptPurposeMint1 -- scriptContextPurpose :: ScriptPurpose

        mockCtx2 :: LedgerApiV2.ScriptContext
        mockCtx2 =
            LedgerApiV2.ScriptContext
                (
                LedgerApiV2.TxInfo
                    mockTxInfoInputs -- txInfoInputs :: [TxInInfo]	
                    [ ] -- txInfoReferenceInputs :: [TxInInfo]
                    mockTxInfoOutputs -- txInfoOutputs :: [TxOut]	
                    (LedgerAda.lovelaceValueOf 50000) -- txInfoFee :: Value	
                    mockTxInfoMint -- txInfoMint :: Value	
                    [] -- txInfoDCert :: [DCert]	
                    (LedgerApiV2.fromList []) -- txInfoWdrl :: Map StakingCredential Integer	
                    validityRange -- txInfoValidRange :: POSIXTimeRange	
                    []  -- txInfoSignatories :: [PubKeyHash]	
                    mockTxInfoRedeemers2 -- txInfoRedeemers :: Map ScriptPurpose Redeemer	
                    (LedgerApiV2.fromList [])  -- txInfoData :: Map DatumHash Datum	
                    (LedgerApiV2.TxId "555") -- txInfoId :: TxId 
                )
                mockScriptPurposeMint2 -- scriptContextPurpose :: ScriptPurpose

        mockCtx3 :: LedgerApiV2.ScriptContext
        mockCtx3 =
            LedgerApiV2.ScriptContext
                (
                LedgerApiV2.TxInfo
                    mockTxInfoInputs -- txInfoInputs :: [TxInInfo]	
                    [ ] -- txInfoReferenceInputs :: [TxInInfo]
                    mockTxInfoOutputs -- txInfoOutputs :: [TxOut]	
                    (LedgerAda.lovelaceValueOf 50000) -- txInfoFee :: Value	
                    mockTxInfoMint -- txInfoMint :: Value	
                    [] -- txInfoDCert :: [DCert]	
                    (LedgerApiV2.fromList []) -- txInfoWdrl :: Map StakingCredential Integer	
                    validityRange -- txInfoValidRange :: POSIXTimeRange	
                    []  -- txInfoSignatories :: [PubKeyHash]	
                    mockTxInfoRedeemers3 -- txInfoRedeemers :: Map ScriptPurpose Redeemer	
                    (LedgerApiV2.fromList [])  -- txInfoData :: Map DatumHash Datum	
                    (LedgerApiV2.TxId "555") -- txInfoId :: TxId 
                )
                mockScriptPurposeMint3 -- scriptContextPurpose :: ScriptPurpose

        !datas1 = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx1]
        !datas2 = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx2]
        !datas3 = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx3]

        (e1, log1, size1) = evaluateScriptMint policy1 datas1
        (e2, log2, size2) = evaluateScriptMint policy2 datas2
        (e3, log3, size3) = evaluateScriptMint policy3 datas3
        (e4, log4, size4) = evaluateScriptMint policy1_fromPlutonomy datas1
        (e5, log5, size5) = evaluateScriptMint policy2_fromPlutonomy datas2
        (e6, log6, size6) = evaluateScriptMint policy3_fromPlutonomy datas3

    in do
        P.putStrLn "-----"
        
        -- P.putStrLn $ "Case: " ++ P.show caseValue 
        -- case e1 of
        --     Left evalErr -> do
        --         P.putStrLn "With (==)"
        --         P.putStrLn $ "Eval Error: " ++  P.show evalErr
        --     Right exbudget -> do 
        --         P.putStrLn "With (==)"
        --         P.putStrLn $ "Ex Budget: " ++  P.show exbudget ++ " - Script size: " ++  P.show size1
        -- P.putStrLn $ "Log: " ++  P.show log1

        -- case e2 of
        --     Left evalErr -> do
        --         P.putStrLn "With valueEqualsValue"
        --         P.putStrLn $ "Eval Error: " ++ P.show evalErr
        --     Right exbudget -> do 
        --         P.putStrLn "With valueEqualsValue"
        --         P.putStrLn $ "Ex Budget: " ++  P.show exbudget ++ " - Script size: " ++  P.show size2
        -- P.putStrLn $ "Log: " ++  P.show log2
        
        -- case e3 of
        --     Left evalErr -> do
        --         P.putStrLn "With unsafeValueEqualsValue"
        --         P.putStrLn $ "Eval Error: " ++ P.show evalErr
        --     Right exbudget -> do 
        --         P.putStrLn "With unsafeValueEqualsValue"
        --         P.putStrLn $ "Ex Budget: " ++  P.show exbudget ++ " - Script size: " ++  P.show size3
        -- P.putStrLn $ "Log: " ++  P.show log3

        -- P.putStrLn "-----"
        
        P.putStrLn $ "Case (with Plutonomy): " ++ P.show caseValue 
        case e4 of
            Left evalErr -> do
                P.putStrLn "With (==)"
                P.putStrLn $ "Eval Error: " ++ P.show evalErr
            Right exbudget -> do 
                P.putStrLn "With (==)"
                P.putStrLn $ "Ex Budget: " ++  P.show exbudget ++ " - Script size: " ++  P.show size4
        P.putStrLn $ "Log: " ++  P.show log4

        case e5 of
            Left evalErr -> do
                P.putStrLn "With valueEqualsValue"
                P.putStrLn $ "Eval Error: " ++ P.show evalErr
            Right exbudget -> do 
                P.putStrLn "With valueEqualsValue"
                P.putStrLn $ "Ex Budget: " ++  P.show exbudget ++ " - Script size: " ++  P.show size5
        P.putStrLn $ "Log: " ++  P.show log5

        case e6 of
            Left evalErr -> do
                P.putStrLn "With unsafeValueEqualsValue"
                P.putStrLn $ "Eval Error: " ++ P.show evalErr
            Right exbudget -> do 
                P.putStrLn "With unsafeValueEqualsValue"
                P.putStrLn $ "Ex Budget: " ++  P.show exbudget ++ " - Script size: " ++  P.show size6
        P.putStrLn $ "Log: " ++  P.show log6


--------------------------------------------------------------------------------
