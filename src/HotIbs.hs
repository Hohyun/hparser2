{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HotIbs
  ( ibsHotParseTest
  ) where

-- import Control.Monad.Combinators
import Data.Text (Text)
import Data.Void
import Data.Char (digitToInt)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Parser = Parsec Void Text

data IBSHOT = IBSHOT
    { bhf01 :: BHF01  -- Only One per HOT file
    , bcs   :: [BC]   -- Many Currency Group
    } deriving (Eq, Show)

data BOH03 = BOH03
    { agtn_boh :: Text
    , rmed_boh :: Text
    , cutp_boh :: Text
    , hred_boh :: Text
    } deriving (Eq, Show)

data BKT06 = BKT06
    { trnn_bkt :: Int
    , trec_bkt :: Int
    , form_bkt :: Text
    , tacn_bkt :: Text
    } deriving (Eq, Show)

data BKS24 = BKS24
    { dais_bks24 :: Text
    , trnn_bks24 :: Int
    , tdnr_bks24 :: Text
    , cdgt_bks24 :: Int
    , cpui_bks24 :: Text
    , cjcp_bks24 :: Text
    , agtn_bks24 :: Text
    , tour_bks24 :: Text
    , trnc_bks24 :: Text
    , todc_bks24 :: Text
    , pnrr_bks24 :: Text
    } deriving (Eq, Show)

-- Document Amounts Record
data BKS30 = BKS30
    { dais_bks30 :: Text
    , trnn_bks30 :: Int
    , tdnr_bks30 :: Text
    , cdgt_bks30 :: Int
    , cobl_bks30 :: Float
    , ntfa_bks30 :: Float
    , tmft1_bks30 :: Text
    , tmfa1_bks30 :: Float
    , tmft2_bks30 :: Text
    , tmfa2_bks30 :: Float
    , tdam_bks30  :: Float
    , lrep_bks30  :: Float
    , cutp_bks30  :: Text
    } deriving (Eq, Show)

-- Commission Record
data BKS39 = BKS39
    { dais_bks39 :: Text
    , trnn_bks39 :: Int
    , tdnr_bks39 :: Text
    , cdgt_bks39 :: Int
    , stat_bks39 :: Text
    , cotp_bks39 :: Text
    , cort_bks39 :: Float
    , coam_bks39 :: Float
    , sptp_bks39 :: Text
    , sprt_bks39 :: Float
    , spam_bks39 :: Float
    , efrt_bks39 :: Float
    , efco_bks39 :: Float
    , apbc_bks39 :: Float
    , cutp_bks39 :: Text
    } deriving (Eq, Show)

-- Qualifying Issue Information for Sales Tranction Record
data BKS46 = BKS46
    { dais_bks46 :: Text
    , trnn_bks46 :: Int
    , tdnr_bks46 :: Text
    , cdgt_bks46 :: Int
    , orin_bks46 :: Text
    , enrs_bks46 :: Text
    } deriving (Eq, Show)

-- Itinerary Data Segment Record
data BKI63 = BKI63
    { dais_bki63 :: Text
    , trnn_bki63 :: Int
    , tdnr_bki63 :: Text
    , cdgt_bki63 :: Int
    , segi_bki63 :: Int
    , stpo_bki63 :: Text
    , nbda_bki63 :: Text
    , nada_bki63 :: Text
    , orac_bki63 :: Text
    , dstc_bki63 :: Text
    , carr_bki63 :: Text
    , ftnr_bki63 :: Text
    , rbkd_bki63 :: Text
    , ftda_bki63 :: Text
    , ftdt_bki63 :: Text
    , fbst_bki63 :: Text
    , fbal_bki63 :: Text
    , fbtd_bki63 :: Text
    , ffrf_bki63 :: Text
    } deriving (Eq, Show)

-- Document Amounts Record
data BAR64 = BAR64
    { dais_bar64 :: Text
    , trnn_bar64 :: Int
    , tdnr_bar64 :: Text
    , cdgt_bar64 :: Int
    , fare_bar64 :: Text
    , tkmi_bar64 :: Text
    , eqfr_bar64 :: Text
    , taxa1_bar64 :: Text
    , taxa2_bar64 :: Text
    , taxa3_bar64 :: Text
    , totl_bar64 :: Text
    , ntsi_bar64 :: Text
    , sasi_bar64 :: Text
    , fcmi_bar64 :: Text
    , baid_bar64 :: Text
    , boon_bar64 :: Text
    , beot_bar64 :: Text
    , fcpi_bar64 :: Text
    } deriving (Eq, Show)

-- Passenger Record
data BAR65 = BAR65
    { dais_bar65 :: Text
    , trnn_bar65 :: Int
    , tdnr_bar65 :: Text
    , cdgt_bar65 :: Int
    , pxnm_bar65 :: Text
    , pxda_bar65 :: Text
    } deriving (Eq, Show)

-- Additional Information: Form of Payment Record
data BAR66 = BAR66
    { dais_bar66 :: Text
    , trnn_bar66 :: Int
    , tdnr_bar66 :: Text
    , cdgt_bar66 :: Int
    , fpsn_bar66 :: Int
    , fpin_bar66 :: Text
    } deriving (Eq, Show)

-- Fare Calculation Record
data BKF81 = BKF81
    { dais_bkf81 :: Text
    , trnn_bkf81 :: Int
    , tdnr_bkf81 :: Text
    , cdgt_bkf81 :: Int
    , frcs_bkf81 :: Int
    , frca_bkf81 :: Text
    } deriving (Eq, Show)

-- Form of Payment Record
data BKP84 = BKP84
    { dais_bkp84 :: Text
    , trnn_bkp84 :: Int
    , fptp_bkp84 :: Text
    , fpam_bkp84 :: Float
    , fpac_bkp84 :: Text
    , exda_bkp84 :: Text
    , aplc_bkp84 :: Text
    , invn_bkp84 :: Text
    , invd_bkp84 :: Text
    , sfam_bkp84 :: Float
    , remt_bkp84 :: Float
    , cutp1_bkp84 :: Text -- reporting currency
    , cvvr_bkp84 :: Text
    , cutp2_bkp84 :: Text -- payment currency
    } deriving (Eq, Show)

-- Currency Group
data BC = BC
    { bch02 :: BCH02
    , bos   :: [BO]
    , bct95 :: BCT95
    , bft99 :: BFT99
    } deriving (Eq, Show)

-- Office Group : per Currency per Agency
data BO = BO
    { boh03   :: BOH03
    , issues  :: Maybe TRXGRP
    , refunds :: Maybe TRXGRP
    , bot94   :: BOT94
    } deriving (Eq, Show)

data TRXGRP = TRXGRP
    { trxs  :: [TRX]
    , bot93 :: Maybe BOT93
    } deriving (Eq, Show)

data TRX = TRX
    { bkt06  :: BKT06
    , bks24s :: [BKS24]
    , bks30s :: [BKS30]
    , bks39  :: BKS39
    , bks46s :: [BKS46]
    , bki63s :: [BKI63]
    , bar64  :: Maybe BAR64
    , bar65  :: Maybe BAR65
    , bar66s :: [BAR66]
    , bkf81s :: [BKF81]
    , bkp84s :: [BKP84]
    } deriving (Eq, Show)

data BHF01 = BHF01
    { bspi_bhf :: Text
    , tacn_bhf :: Text
    , revn_bhf :: Text
    , prda_bhf :: Text
    , time_bhf :: Text
    , isoc_bhf :: Text
    , fsqn_bhf :: Text
    } deriving (Eq, Show)

data BCH02 = BCH02
    { pdai_bch :: Text
    , pcyc_bch :: Int
    , baed_bch :: Text
    , hred_bch :: Text
    } deriving (Eq, Show)

-- Office Subtotals per Transaction Code and Currency Type Record
data BOT93 = BOT93
    { agtn_bot93 :: Text
    , rmed_bot93 :: Text
    , gros_bot93 :: Float
    , trem_bot93 :: Float
    , tcom_bot93 :: Float
    , ttmf_bot93 :: Float
    , tlrp_bot93 :: Float
    , trnc_bot93 :: Text
    , ttca_bot93 :: Float
    , cutp_bot93 :: Text
    } deriving (Eq, Show)

-- Office Totals per Currency Type Record
data BOT94 = BOT94
    { agtn_bot94 :: Text
    , rmed_bot94 :: Text
    , gros_bot94 :: Float
    , trem_bot94 :: Float
    , tcom_bot94 :: Float
    , ttmf_bot94 :: Float
    , tlrp_bot94 :: Float
    , ttca_bot94 :: Float
    , cutp_bot94 :: Text
    } deriving (Eq, Show)

data BCT95 = BCT95
    { pdai_bct95 :: Text
    , pcyc_bct95 :: Int
    , ofcc_bct95 :: Int
    , gros_bct95 :: Float
    , trem_bct95 :: Float
    , tcom_bct95 :: Float
    , ttmf_bct95 :: Float
    , tlrp_bct95 :: Float
    , ttca_bct95 :: Float
    , cutp_bct95 :: Text
    } deriving (Eq, Show)

data BFT99 = BFT99
    { bspi_bft99 :: Text
    , ofcc_bft99 :: Int
    , gros_bft99 :: Float
    , trem_bft99 :: Float
    , tcom_bft99 :: Float
    , ttmf_bft99 :: Float
    , tlrp_bft99 :: Float
    , ttca_bft99 :: Float
    , cutp_bft99 :: Text
    } deriving (Eq, Show)

bhfParser :: Parser BHF01
bhfParser = do
    bspi_bhf <- string "BFH" >> takeP Nothing 8 >> string "01" >> T.pack <$> count 3 alphaNumChar
    tacn_bhf <- T.pack <$> count 3 alphaNumChar <* count 2 hspace
    revn_bhf <- T.pack <$> count 3 digitChar <* string "PROD"
    prda_bhf <- T.pack <$> count 6 digitChar
    time_bhf <- T.pack <$> count 4 digitChar
    isoc_bhf <- T.pack <$> count 2 letterChar
    fsqn_bhf <- T.pack <$> count 6 digitChar <* manyTill anySingle eol
    return BHF01 {..}

bch02Parser :: Parser BCH02
bch02Parser = do
    pdai_bch <- string "BCH" >> takeP Nothing 10 >> T.pack <$> count 3 alphaNumChar
    pcyc_bch <- read <$> count 1 digitChar
    baed_bch <- T.pack <$> count 6 digitChar <* char 'F'
    hred_bch <- T.pack <$> count 6 digitChar <* manyTill anySingle eol
    return BCH02 {..}

boh03Parser :: Parser BOH03
boh03Parser = do
    agtn_boh <- string "BOH" >> takeP Nothing 10 >> T.pack <$> count 8 digitChar
    rmed_boh <- T.pack <$> count 6 digitChar <* takeP Nothing 23
    cutp_boh <- T.pack <$> count 4 alphaNumChar
    hred_boh <- T.strip . T.pack <$> count 3 printChar <* manyTill anySingle eol
    return BOH03 {..}

bkt06Parser :: Parser BKT06
bkt06Parser = do
    trnn_bkt <- string "BKT" >> takeP Nothing 16 >> read <$> count 6 digitChar
    trec_bkt <- takeP Nothing 20 >> read <$> count 3 digitChar
    form_bkt <- T.pack <$> count 1 alphaNumChar <* anySingle
    tacn_bkt <- T.pack <$> count 3 printChar <* manyTill anySingle eol
    return BKT06 {..}

bks24Parser :: Parser BKS24
bks24Parser = do
    dais_bks24 <- string "BKS" >> count 8 digitChar >> string "24" >> T.pack <$> count 6 digitChar
    trnn_bks24 <- read <$> count 6 digitChar
    tdnr_bks24 <- T.pack <$> count 13 digitChar <* count 2 anySingle
    cdgt_bks24 <- read <$> count 1 digitChar <* takeP Nothing 16
    cpui_bks24 <- T.pack <$> count 4 alphaNumChar
    cjcp_bks24 <- T.strip . T.pack <$> count 3 (alphaNumChar <|> char ' ')
    agtn_bks24 <- T.pack <$> count 8 digitChar <* takeP Nothing 5
    tour_bks24 <- T.strip <$> T.pack <$> count 15 (alphaNumChar <|> char ' ')
    trnc_bks24 <- T.pack <$> count 4 alphaNumChar
    todc_bks24 <- T.pack <$> count 6 alphaNumChar <* count 12 anySingle
    pnrr_bks24 <- T.strip . T.pack <$> count 13 (alphaNumChar <|> char ' ') <* manyTill anySingle eol
    return BKS24 {..}

bks30Parser :: Parser BKS30
bks30Parser = do
    dais_bks30 <- string "BKS" >> count 8 digitChar >> string "30" >> T.pack <$> count 6 digitChar
    trnn_bks30 <- read <$> count 6 digitChar
    tdnr_bks30 <- T.pack <$> count 13 digitChar <* count 2 anySingle
    cdgt_bks30 <- read <$> count 1 digitChar
    cobl_ <- T.pack <$> count 11 printChar
    ntfa_ <- T.pack <$> count 11 printChar <* anySingle
    tmft1_bks30 <- T.strip . T.pack <$> count 8 printChar
    tmfa1_ <- T.pack <$> count 11 printChar
    tmft2_bks30 <- T.strip . T.pack <$> count 8 printChar
    tmfa2_ <- T.pack <$> count 11 printChar
    tdam_ <- T.pack <$> count 11 printChar
    lrep_ <- T.pack <$> count 11 printChar <* takeP Nothing 5
    cutp_bks30 <- T.pack <$> count 4 alphaNumChar <* manyTill anySingle eol
    let cobl_bks30 = parseAmount cutp_bks30 cobl_
    let ntfa_bks30 = parseAmount cutp_bks30 ntfa_
    let tmfa1_bks30 = parseAmount cutp_bks30 tmfa1_
    let tmfa2_bks30 = parseAmount cutp_bks30 tmfa2_
    let tdam_bks30 = parseAmount cutp_bks30 tdam_
    let lrep_bks30  = parseAmount cutp_bks30 lrep_
    return BKS30 {..}

bks39Parser :: Parser BKS39
bks39Parser = do
    dais_bks39 <- string "BKS" >> count 8 digitChar >> string "39" >> T.pack <$> count 6 digitChar
    trnn_bks39 <- read <$> count 6 digitChar
    tdnr_bks39 <- T.pack <$> count 13 digitChar <* count 2 anySingle
    cdgt_bks39 <- read <$> count 1 digitChar
    stat_bks39 <- T.strip . T.pack <$> count 3 printChar
    cotp_bks39 <- T.strip . T.pack <$> count 6 printChar
    cort_bks39 <- read <$> count 5 digitChar
    coam_ <- T.pack <$> count 11 printChar
    sptp_bks39 <- T.strip . T.pack <$> count 6 printChar
    sprt_bks39 <- read <$> count 5 digitChar
    spam_ <- T.pack <$> count 11 printChar
    efrt_bks39 <- read <$> count 5 digitChar
    efco_ <- T.pack <$> count 11 printChar
    apbc_ <- T.pack <$> count 11 printChar <* takeP Nothing 14
    cutp_bks39 <- T.pack <$> count 4 alphaNumChar <* manyTill anySingle eol
    let coam_bks39 = parseAmount cutp_bks39 coam_
    let spam_bks39 = parseAmount cutp_bks39 spam_
    let efco_bks39 = parseAmount cutp_bks39 efco_
    let apbc_bks39 = parseAmount cutp_bks39 apbc_
    return BKS39 {..}

bks46Parser :: Parser BKS46
bks46Parser = do
    dais_bks46 <- string "BKS" >> count 8 digitChar >> string "46" >> T.pack <$> count 6 digitChar
    trnn_bks46 <- read <$> count 6 digitChar
    tdnr_bks46 <- T.pack <$> count 13 digitChar <* count 2 anySingle
    cdgt_bks46 <- read <$> count 1 digitChar
    orin_bks46 <- T.strip . T.pack <$> count 32 printChar
    enrs_bks46 <- T.strip . T.pack <$> count 58 printChar <* manyTill anySingle eol
    return BKS46 {..}

bki63Parser :: Parser BKI63
bki63Parser = do
    dais_bki63 <- string "BKI" >> count 8 digitChar >> string "63" >> T.pack <$> count 6 digitChar
    trnn_bki63 <- read <$> count 6 digitChar
    tdnr_bki63 <- T.strip . T.pack <$> count 13 digitChar <* count 2 anySingle
    cdgt_bki63 <- read <$> count 1 digitChar
    segi_bki63 <- read <$> count 1 digitChar
    stpo_bki63 <- T.strip . T.pack <$> count 1 printChar
    nbda_bki63 <- T.strip . T.pack <$> count 5 printChar
    nada_bki63 <- T.strip . T.pack <$> count 5 printChar <* takeP Nothing 7
    orac_bki63 <- T.strip . T.pack <$> count 5 printChar
    dstc_bki63 <- T.strip . T.pack <$> count 5 printChar
    carr_bki63 <- T.strip . T.pack <$> count 4 printChar <* anySingle
    ftnr_bki63 <- T.strip . T.pack <$> count 5 printChar
    rbkd_bki63 <- T.strip . T.pack <$> count 2 printChar
    ftda_bki63 <- T.pack <$> count 5 alphaNumChar <* anySingle
    ftdt_bki63 <- T.pack <$> count 5 alphaNumChar <* takeP Nothing 3
    fbst_bki63 <- T.pack <$> count 2 letterChar
    fbal_bki63 <- T.strip . T.pack <$> count 3 printChar <* anySingle
    fbtd_bki63 <- T.strip . T.pack <$> count 15 printChar
    ffrf_bki63 <- T.strip . T.pack <$> count 16 printChar <* manyTill anySingle eol
    return BKI63 {..}

bar64Parser :: Parser BAR64
bar64Parser = do
    dais_bar64 <- string "BAR" >> count 8 digitChar >> string "64" >> T.pack <$> count 6 digitChar
    trnn_bar64 <- read <$> count 6 digitChar
    tdnr_bar64 <- T.pack <$> count 13 digitChar <* count 2 anySingle
    cdgt_bar64 <- read <$> count 1 digitChar
    fare_bar64 <- T.strip . T.pack <$> count 11 printChar
    tkmi_bar64 <- T.pack <$> count 1 alphaNumChar
    eqfr_bar64 <- T.strip . T.pack <$> count 11 printChar
    taxa1_bar64 <- T.strip . T.pack <$> count 11 printChar
    taxa2_bar64 <- T.strip . T.pack <$> count 11 printChar
    taxa3_bar64 <- T.strip . T.pack <$> count 11 printChar
    totl_bar64 <- T.strip . T.pack <$> count 11 printChar
    ntsi_bar64 <- T.strip . T.pack <$> count 4 printChar
    sasi_bar64 <- T.strip . T.pack <$> count 4 printChar
    fcmi_bar64 <- T.strip . T.pack <$> count 1 printChar
    baid_bar64 <- T.strip . T.pack <$> count 6 printChar
    boon_bar64 <- T.strip . T.pack <$> count 10 printChar
    beot_bar64 <- T.strip . T.pack <$> count 1 printChar
    fcpi_bar64 <- T.strip . T.pack <$> count 1 printChar <* manyTill anySingle eol
    return BAR64 {..}

bar65Parser :: Parser BAR65
bar65Parser = do
    dais_bar65 <- string "BAR" >> count 8 digitChar >> string "65" >> T.pack <$> count 6 digitChar
    trnn_bar65 <- read <$> count 6 digitChar
    tdnr_bar65 <- T.pack <$> count 13 digitChar <* count 2 anySingle
    cdgt_bar65 <- read <$> count 1 digitChar
    pxnm_bar65 <- T.strip . T.pack <$> count 49 printChar
    pxda_bar65 <- T.strip . T.pack <$> count 46 printChar <* eol
    return BAR65 {..}

bar66Parser :: Parser BAR66
bar66Parser = do
    dais_bar66 <- string "BAR" >> count 8 digitChar >> string "66" >> T.pack <$> count 6 digitChar
    trnn_bar66 <- read <$> count 6 digitChar
    tdnr_bar66 <- T.pack <$> count 13 digitChar <* count 2 anySingle
    cdgt_bar66 <- read <$> count 1 digitChar
    fpsn_bar66 <- read <$> count 1 digitChar
    fpin_bar66 <- T.strip . T.pack <$> count 50 printChar <* manyTill anySingle eol
    return BAR66 {..}

bkf81Parser :: Parser BKF81
bkf81Parser = do
    dais_bkf81 <- string "BKF" >> count 8 digitChar >> string "81" >> T.pack <$> count 6 digitChar
    trnn_bkf81 <- read <$> count 6 digitChar
    tdnr_bkf81 <- T.pack <$> count 13 digitChar <* count 2 anySingle
    cdgt_bkf81 <- read <$> count 1 digitChar <* count 2 anySingle
    frcs_bkf81 <- read <$> count 1 digitChar
    frca_bkf81 <- T.strip . T.pack <$> count 87 printChar <* manyTill anySingle eol
    return BKF81 {..}

bkp84Parser :: Parser BKP84
bkp84Parser = do
    dais_bkp84 <- string "BKP" >> count 8 digitChar >> string "84" >> T.pack <$> count 6 digitChar
    trnn_bkp84 <- read <$> count 6 digitChar
    fptp_bkp84 <- T.strip . T.pack <$> count 10 printChar
    fpam_ <- T.pack <$> count 11 printChar
    fpac_bkp84 <- T.strip . T.pack <$> count 19 printChar
    exda_bkp84 <- T.strip . T.pack <$> count 4 printChar <* count 5 anySingle
    aplc_bkp84 <- T.strip . T.pack <$> count 6 printChar
    invn_bkp84 <- T.strip . T.pack <$> count 14 printChar
    invd_bkp84 <- T.strip . T.pack <$> count 6 printChar
    sfam_ <- T.pack <$> count 11 printChar
    remt_ <- T.pack <$> count 11 printChar
    cutp1_bkp84 <- T.pack <$> count 4 printChar
    cvvr_bkp84 <- T.strip . T.pack <$> count 1 printChar <* count 2 anySingle
    cutp2_bkp84 <- T.pack <$> count 4 printChar <* manyTill anySingle eol
    let fpam_bkp84 = parseAmount cutp2_bkp84 fpam_
    let sfam_bkp84 = parseAmount cutp2_bkp84 sfam_
    let remt_bkp84 = parseAmount cutp2_bkp84 remt_
    return BKP84 {..}

bot93Parser :: Parser BOT93
bot93Parser = do
    agtn_bot93 <- string "BOT" >> count 8 digitChar >> string "93" >> T.pack <$> count 8 digitChar
    rmed_bot93 <- T.pack <$> count 6 digitChar <* count 7 anySingle
    gros_ <- T.pack <$> count 15 printChar
    trem_ <- T.pack <$> count 15 printChar
    tcom_ <- T.pack <$> count 15 printChar
    ttmf_ <- T.pack <$> count 15 printChar
    tlrp_ <- T.pack <$> count 15 printChar
    trnc_bot93 <- T.pack <$> count 4 alphaNumChar <* anySingle
    ttca_ <- T.pack <$> count 15 printChar
    cutp_bot93 <- T.pack <$> count 4 alphaNumChar <* manyTill anySingle eol
    let gros_bot93 = parseAmount cutp_bot93 gros_
    let trem_bot93 = parseAmount cutp_bot93 trem_
    let tcom_bot93 = parseAmount cutp_bot93 tcom_
    let ttmf_bot93 = parseAmount cutp_bot93 ttmf_
    let tlrp_bot93 = parseAmount cutp_bot93 tlrp_
    let ttca_bot93 = parseAmount cutp_bot93 ttca_
    return BOT93 {..}

bot94Parser :: Parser BOT94
bot94Parser = do
    agtn_bot94 <- string "BOT" >> count 8 digitChar >> string "94" >> T.pack <$> count 8 digitChar
    rmed_bot94 <- T.pack <$> count 6 digitChar <* count 7 anySingle
    gros_ <- T.pack <$> count 15 printChar
    trem_ <- T.pack <$> count 15 printChar
    tcom_ <- T.pack <$> count 15 printChar
    ttmf_ <- T.pack <$> count 15 printChar
    tlrp_ <- T.pack <$> count 15 printChar <* count 5 anySingle
    ttca_ <- T.pack <$> count 15 printChar
    cutp_bot94 <- T.pack <$> count 4 alphaNumChar <* manyTill anySingle eol
    let gros_bot94 = parseAmount cutp_bot94 gros_
    let trem_bot94 = parseAmount cutp_bot94 trem_
    let tcom_bot94 = parseAmount cutp_bot94 tcom_
    let ttmf_bot94 = parseAmount cutp_bot94 ttmf_
    let tlrp_bot94 = parseAmount cutp_bot94 tlrp_
    let ttca_bot94 = parseAmount cutp_bot94 ttca_
    return BOT94 {..}

bct95Parser :: Parser BCT95
bct95Parser = do
    pdai_bct95 <- string "BCT" >> count 8 digitChar >> string "95" >> T.pack <$> count 3 alphaNumChar
    pcyc_bct95 <- read <$> count 1 digitChar <* count 7 anySingle
    ofcc_bct95 <- read <$> count 5 digitChar
    gros_ <- T.pack <$> count 15 printChar
    trem_ <- T.pack <$> count 15 printChar
    tcom_ <- T.pack <$> count 15 printChar
    ttmf_ <- T.pack <$> count 15 printChar
    tlrp_ <- T.pack <$> count 15 printChar <* count 10 anySingle
    ttca_ <- T.pack <$> count 15 printChar
    cutp_bct95 <- T.pack <$> count 4 alphaNumChar <* manyTill anySingle eol
    let gros_bct95 = parseAmount cutp_bct95 gros_
    let trem_bct95 = parseAmount cutp_bct95 trem_
    let tcom_bct95 = parseAmount cutp_bct95 tcom_
    let ttmf_bct95 = parseAmount cutp_bct95 ttmf_
    let tlrp_bct95 = parseAmount cutp_bct95 tlrp_
    let ttca_bct95 = parseAmount cutp_bct95 ttca_
    return BCT95 {..}

bft99Parser :: Parser BFT99
bft99Parser = do
    bspi_bft99 <- string "BFT" >> count 8 digitChar >> string "99" >> T.pack <$> count 3 alphaNumChar <* count 7 anySingle
    ofcc_bft99 <- read <$> count 5 digitChar
    gros_ <- T.pack <$> count 15 printChar
    trem_ <- T.pack <$> count 15 printChar
    tcom_ <- T.pack <$> count 15 printChar
    ttmf_ <- T.pack <$> count 15 printChar
    tlrp_ <- T.pack <$> count 15 printChar <* count 11 anySingle
    ttca_ <- T.pack <$> count 15 printChar
    cutp_bft99 <- T.pack <$> count 4 alphaNumChar <* manyTill anySingle eol
    let gros_bft99 = parseAmount cutp_bft99 gros_
    let trem_bft99 = parseAmount cutp_bft99 trem_
    let tcom_bft99 = parseAmount cutp_bft99 tcom_
    let ttmf_bft99 = parseAmount cutp_bft99 ttmf_
    let tlrp_bft99 = parseAmount cutp_bft99 tlrp_
    let ttca_bft99 = parseAmount cutp_bft99 ttca_
    return BFT99 {..}

-- Composite Parsers
trxParser :: Parser TRX
trxParser = do
    bkt06 <- bkt06Parser
    bks24s <- some (try bks24Parser)
    bks30s <- some (try bks30Parser)
    bks39 <- try bks39Parser
    bks46s <- many (try bks46Parser)
    bki63s <- many (try bki63Parser)
    bar64 <- optional (try bar64Parser)
    bar65 <- optional (try bar65Parser)
    bar66s <- many (try bar66Parser)
    bkf81s <- many (try bkf81Parser)
    bkp84s <- many (try bkp84Parser)
    return TRX {..}

trxgrpParser :: Parser TRXGRP
trxgrpParser = do
    trxs <- many (try trxParser)
    bot93 <- optional (try bot93Parser)
    return TRXGRP {..}

boParser :: Parser BO
boParser = do
    boh03 <- boh03Parser
    issues <- optional (try trxgrpParser)
    refunds <- optional (try trxgrpParser)
    bot94 <- bot94Parser
    return BO {..}

bcParser :: Parser BC
bcParser = do
    bch02 <- bch02Parser
    bos <- many (try boParser)
    bct95 <- bct95Parser
    bft99 <- bft99Parser
    return BC {..}

ibsHotParser :: Parser IBSHOT
ibsHotParser = do
    bhf01 <- bhfParser
    bcs <- many bcParser <* eof
    return IBSHOT {..}

parseAmount :: Text -> Text -> Float
parseAmount cutp xs =
    let ls = digitToInt $ T.last cutp
        d = 10^ls
    in if l >= 0
       then (h * 10 + fromIntegral l) / d :: Float
       else -1 * (h * 10 + abs (fromIntegral l)) / d :: Float
  where h = read $ init $ T.unpack xs :: Float
        l = case last $ T.unpack xs of
              '{' -> 0 :: Int
              'A' -> 1 :: Int
              'B' -> 2 :: Int
              'C' -> 3 :: Int
              'D' -> 4 :: Int
              'E' -> 5 :: Int
              'F' -> 6 :: Int
              'G' -> 7 :: Int
              'H' -> 8 :: Int
              'I' -> 9 :: Int
              '}' -> 0 :: Int
              'J' -> -1 :: Int
              'K' -> -2 :: Int
              'L' -> -3 :: Int
              'M' -> -4 :: Int
              'N' -> -5 :: Int
              'O' -> -6 :: Int
              'P' -> -7 :: Int
              'Q' -> -8 :: Int
              'R' -> -9 :: Int
              _ -> 0 :: Int

ibsHotParseTest :: IO ()
ibsHotParseTest = do
    putStrLn "IBS HOT Parser"
    input <- TIO.readFile "./hot_ibs_test.txt"
    parseTest ibsHotParser input
