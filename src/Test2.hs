{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Use camelCase" -}


module Test2 where

import qualified Data.ByteString.Short                         as DataByteStringShort
import qualified Data.ByteString.Lazy                          as DataByteStringLazy
import qualified Codec.Serialise                               as CodecSerialise
import qualified Data.Maybe                                    as DataMaybe
import qualified Ledger
import qualified Ledger.Ada                                    as LedgerAda
import qualified Ledger.Address                                as LedgerAddress
import qualified Ledger.Value                                  as LedgerValue
-- import qualified Plutonomy
-- import qualified Plutus.Script.Utils.V2.Scripts                as UtilsScriptsV2
import qualified Plutus.V1.Ledger.Scripts                      as LedgerScriptsV1
import qualified Plutus.V2.Ledger.Api                          as LedgerApiV2
-- import qualified Plutus.V2.Ledger.Contexts                     as LedgerContextsV2
import qualified Plutus.V2.Ledger.EvaluationContext            as LedgerEvaluationContextV2
-- import qualified PlutusTx
-- import qualified PlutusTx.AssocMap                             as TxAssocMap
-- import           PlutusTx.Prelude
import qualified Prelude                                       as P
-- import qualified PlutusTx.Builtins                             as TxBuiltins

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($), (&&))

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.Typed as Scripts
import Plutus.V2.Ledger.Api qualified as Plutus
import Plutus.V2.Ledger.Contexts as V2
import PlutusTx qualified
import PlutusTx.Builtins
import PlutusTx.Eq as PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))
import PlutusTx.Prelude qualified as PlutusPrelude

-- serialiseData is a PlutusV2 builtin

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> V2.ScriptContext -> Bool
mkValidator _ redeemer sc =
  serialiseData redeemer PlutusTx./= emptyByteString &&
  PlutusPrelude.isJust (PlutusPrelude.find
    (PlutusTx.== Plutus.OutputDatum (Plutus.Datum $ PlutusTx.toBuiltinData (42 :: Integer)))
    txinsDatums) &&
  PlutusPrelude.isJust (PlutusPrelude.find
    (PlutusTx.== Plutus.OutputDatum (Plutus.Datum $ PlutusTx.toBuiltinData (42 :: Integer)))
    referenceInputDatums)
 where
  txInfo = V2.scriptContextTxInfo sc
  txinsDatums = PlutusPrelude.map (txOutDatum . txInInfoResolved)
                  $ V2.txInfoInputs txInfo
  referenceInputDatums =
    PlutusPrelude.map (txOutDatum . txInInfoResolved)
      $ V2.txInfoReferenceInputs txInfo

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.mkUntypedValidator mkValidator

script :: Plutus.Script
script = Plutus.unValidatorScript validator

requireRedeemerScriptShortBs :: SBS.ShortByteString
requireRedeemerScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

requireRedeemerScript :: PlutusScript PlutusScriptV2
requireRedeemerScript = PlutusScriptSerialised requireRedeemerScriptShortBs

---------------------------------------------------

evaluateScriptValidator :: LedgerApiV2.Validator -> [PlutusTx.Data] -> (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Integer)
evaluateScriptValidator validator' datas =
    let
        !pv = LedgerApiV2.ProtocolVersion 6 0

        getScriptShortBs :: LedgerApiV2.Script -> DataByteStringShort.ShortByteString
        getScriptShortBs = DataByteStringShort.toShort . DataByteStringLazy.toStrict . CodecSerialise.serialise

        !scriptUnValidatorV2 = LedgerApiV2.unValidatorScript validator'

        !scriptShortBsV2 = getScriptShortBs scriptUnValidatorV2
        --scriptSerialisedV2 = Utils.getScriptSerialised scriptShortBsV2

        !(logout, e) = LedgerApiV2.evaluateScriptCounting pv LedgerApiV2.Verbose LedgerEvaluationContextV2.evalCtxForTesting scriptShortBsV2 datas

        !size = LedgerScriptsV1.scriptSize scriptUnValidatorV2

    in 
        (logout, e, size)
  

---------------------------------------------------

evaluate :: P.IO ()
evaluate = 
    let
        exampleTxOutRef :: LedgerApiV2.TxOutRef
        exampleTxOutRef = LedgerApiV2.TxOutRef {
            LedgerApiV2.txOutRefId = "aaccff",
            LedgerApiV2.txOutRefIdx = 10
        }

        exampleAddress :: LedgerAddress.Address
        exampleAddress =  LedgerAddress.Address {LedgerApiV2.addressCredential =  LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash  "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e" , LedgerApiV2.addressStakingCredential =  Nothing }

        value1 = LedgerAda.lovelaceValueOf 0

        mockInput1 :: LedgerApiV2.TxInInfo
        mockInput1 =
            LedgerApiV2.TxInInfo
                exampleTxOutRef
                (LedgerApiV2.TxOut
                    exampleAddress -- txOutAddress :: Address	 
                    value1 -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ()) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockInput2 :: LedgerApiV2.TxInInfo
        mockInput2 =
            LedgerApiV2.TxInInfo
                exampleTxOutRef
                (LedgerApiV2.TxOut
                    exampleAddress -- txOutAddress :: Address	 
                    value1 -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ()) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )        

        mockOutput1 :: LedgerApiV2.TxOut
        mockOutput1 =
            LedgerApiV2.TxOut
                exampleAddress -- txOutAddress :: Address	 
                value1 -- txOutValue :: Value	 
                (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ()) -- txOutDatum :: OutputDatum	
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        --------------------------------

        redeemer = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData ()

        mockTxInfoInputs :: [LedgerApiV2.TxInInfo]
        mockTxInfoInputs = [ mockInput1, mockInput2 ]

        mockTxInfoOutputs :: [LedgerApiV2.TxOut]
        mockTxInfoOutputs = [ mockOutput1 ]

        mockTxInfoMint :: LedgerValue.Value
        mockTxInfoMint = LedgerAda.lovelaceValueOf 0

        mockScriptPurposeSpent :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeSpent = LedgerApiV2.Spending exampleTxOutRef

        mockRedeemer :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemer = (mockScriptPurposeSpent, redeemer)

        mockTxInfoRedeemers1 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers1 = LedgerApiV2.fromList
            [
                mockRedeemer
            ]

        !now = LedgerApiV2.POSIXTime 1000000
        !intervalOffset1 = 1000
        !intervalOffset2 = 15000
        !validityRange   = Ledger.interval ( now P.- intervalOffset1 ) (now P.+ intervalOffset2)

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
                mockScriptPurposeSpent -- scriptContextPurpose :: ScriptPurpose

        !datas1 = [ LedgerApiV2.toData redeemer, LedgerApiV2.toData mockCtx1]

        (_,e1, size1) = evaluateScriptValidator validator datas1

    in do
        P.putStrLn "-----"
        case e1 of
            Left evalErr -> do
                P.putStrLn $ "Eval Error: " P.++  P.show evalErr
            Right exbudget -> do 
                P.putStrLn $ "Ex Budget: " P.++  P.show exbudget P.++ " - Script size: " P.++  P.show size1

       