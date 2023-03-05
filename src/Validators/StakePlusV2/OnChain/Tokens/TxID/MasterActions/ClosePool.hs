{-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE BangPatterns #-}
-- -- {-# LANGUAGE Strict #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.ClosePool 
(
    policy_TxID_Master_ClosePool 
) 
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Value                                               as LedgerValue (AssetClass (AssetClass), assetClassValue)
import qualified Plutonomy  
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy)
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo)
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), traceIfFalse, length )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (mkUpdated_PoolDatum_With_ClosedAt, valueEqualsValue, getPoolDatumTypo_FromDatum) 
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getOutputsWithDatum, validateMasterAction, isNFT_Minted_With_AC, isDateInRange, isNotClosed) 
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (getTxOut_Datum, validateBurn_Token_Own_CS_Any_TN, getTxOut_Value, checkIfAllSpendRedeemersAreEqual, checkIfAllAreFromSameAddress, getTxOut_Value_And_SomeDatum) 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, txID_Master_ClosePool_TN, const_1_PD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum)
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerMasterClosePool), RedeemerMasterClosePoolTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_ClosePool #-}
mkPolicy_TxID_Master_ClosePool :: T.PoolParams -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_ClosePool !pParams !mintRedeemerRaw !ctxRaw  = 
   let
        !mintRedeemer = LedgerApiV2.unsafeFromBuiltinData @T.Redeemer_TxID mintRedeemerRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    in
        if
            case mintRedeemer of
                (T.RedeemerBurn_TxID T.RedeemerBurn_TxIDTypo) -> OnChainNFTHelpers.validateBurn_Token_Own_CS_Any_TN ctx
                (T.RedeemerMint_TxID T.RedeemerMint_TxIDTypo {..}) ->
                    let
                        !redeemer' = mrRedeemerValidator
                        !inputs_WithDatum = OnChainHelpers.getInputsWithDatum ctx
                        !outputs_WithDatum = OnChainHelpers.getOutputsWithDatum ctx
                        !inputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum ]
                        !outputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- outputs_WithDatum ]
                    in
                        traceIfFalse "INVIO" (OnChainNFTHelpers.checkIfAllAreFromSameAddress inputs_WithDatum outputs_WithDatum) && 
                        traceIfFalse "INVR" (OnChainNFTHelpers.checkIfAllSpendRedeemersAreEqual ctx redeemer') && 

                        case redeemer' of
                            (T.RedeemerMasterClosePool redeemer) ->
                                    validateMasterClosePool pParams ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                            _ -> traceError "INVOP"
        then ()
        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE validateMasterClosePool #-}
validateMasterClosePool :: T.PoolParams -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterClosePoolTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool 
validateMasterClosePool !pParams !ctx !redeemer !inputs_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums  =
        OnChainHelpers.validateMasterAction pParams info master &&
        traceIfFalse "WIO" correctIO &&
        traceIfFalse "MCP" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_ClosePool_AC  info ) &&

        traceIfFalse "CLOSED" (OnChainHelpers.isNotClosed pParams info poolDatum_In )  &&
        
        traceIfFalse "DATE" (OnChainHelpers.isDateInRange closedAt info) &&
        traceIfFalse "PD" correctOutput_PoolDatum_Updated_With_ClosedAt &&
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_WithTokens
    where
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmcpMaster redeemer
        !closedAt = T.rmcpClosedAt redeemer
        ------------------
        !txID_Master_ClosePool_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_ClosePool_AC =  LedgerValue.AssetClass (txID_Master_ClosePool_CS, T.txID_Master_ClosePool_TN)
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !input_TxOut_Value_And_PoolDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                Nothing -> traceError "IPD"
                Just x  -> x        
        ------------------
        !output_TxOut_Value_And_PoolDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                Nothing -> traceError "OPD"
                Just x  -> x
        ------------------
        correctIO :: Bool
        correctIO =
            -- caseIO # : REF INPUTS , NORMAL INPUTS      / OUTPUTS
            -- caseIO 1 : 0 R        , 1 (1 PD)           / 1 (1 PD)
            (length inputs_TxOut_Values_And_Datums  == T.const_1_PD ) &&
            (length outputs_TxOut_Values_And_Datums == T.const_1_PD )
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
        ------------------
        correctOutput_PoolDatum_Updated_With_ClosedAt :: Bool
        !correctOutput_PoolDatum_Updated_With_ClosedAt =
            let
                !poolDatum_Out_Control = Helpers.mkUpdated_PoolDatum_With_ClosedAt poolDatum_In closedAt 
                !poolDatum_Out_Real = OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
            in  
                poolDatum_Out_Real == poolDatum_Out_Control
        ------------------
        correctOutput_PoolDatum_Value_WithTokens :: Bool
        !correctOutput_PoolDatum_Value_WithTokens =
            let
                !value_In_PoolDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum
            ---------------------
                !value_For_Mint_TxID_Master_ClosePool = LedgerValue.assetClassValue txID_Master_ClosePool_AC 1
            ---------------------
                !value_For_PoolDatum_Control = value_In_PoolDatum <> value_For_Mint_TxID_Master_ClosePool
                !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
            in  
                Helpers.valueEqualsValue value_For_PoolDatum_Real value_For_PoolDatum_Control
        ------------------

--------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_Master_ClosePool #-}
policy_TxID_Master_ClosePool :: T.PoolParams -> LedgerApiV2.MintingPolicy
policy_TxID_Master_ClosePool pParams = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> Plutonomy.MintingPolicy
original_policy pParams =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_Master_ClosePool ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams

--------------------------------------------------------------------------------
