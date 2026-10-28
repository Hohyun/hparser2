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

data HOTIBS = HOTIBS
    { bhf01  :: BHF01
    , bch02  :: BCH02
    , boh03  :: BOH03
    , bkt06  :: BKT06
    , bks24s :: [BKS24]
    , bks30s :: [BKS30]
    , bks39  :: BKS39
    , bks46s :: [BKS46]
    , bki63s :: [BKI63]
    --, bft99 :: BFT99
    } deriving (Eq, Show)

-- data BC = BC
--     { bch02 :: BCH02
--     , bos   :: [BO]
--     , bct95 :: BCT95
--     } deriving (Eq, Show)

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


-- data BO = BO
--     { boh03   :: BOH03
--     , issues  :: TRXS
--     , refunds :: TRXS
--     , bot94   :: BOT94
--     } deriving (Eq, Show)

-- data TRXS = TRXS
--     { tickets  :: [TRX]
--     , bot93 :: BOT93
--     } deriving (Eq, Show)
--
-- data TRX = TRX
--     {
--
--     } deriving (Eq, Show)

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

data BOT93 = BOT93
    {

    } deriving (Eq, Show)

data BOT94 = BOT94
    {

    } deriving (Eq, Show)

data BCT95 = BCT95
    {

    } deriving (Eq, Show)

-- data BFT99 = BFT99
--     { ofcc :: Integer
--     , gros :: Float
--     , trem :: Float
--     , tcom :: Float
--     , ttmf :: Float
--     , tlrp :: Float
--     , ttca :: Float
--     , cutp_bft :: Text
--     } deriving (Eq, Show)

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

-- bftParser :: Parser BFT99
-- bftParser = do
--     _ <- string "BFT" >> takeP Nothing 8 >> string "99" >> takeP Nothing 10
--     ofcc_ <- count 5 digitChar
--     gros_ <- count 15 printChar
--     trem_ <- count 15 printChar
--     tcom_ <- count 15 printChar
--     ttmf_ <- count 15 printChar
--     tlrp_ <- count 15 printChar <* hspace
--     ttca_ <- count 15 printChar
--     cutp  <- T.pack <$> count 4 alphaNumChar <* space <* eof
--     let ofcc = read ofcc_ :: Integer
--         gros = stringToFloat gros_
--         trem = stringToFloat trem_
--         tcom = stringToFloat tcom_
--         ttmf = stringToFloat ttmf_
--         tlrp = stringToFloat tlrp_
--         ttca = stringToFloat ttca_
--     return BFT99 {..}

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
    dais_bks24 <- try $ string "BKS" >> count 8 digitChar >> string "24" >> T.pack <$> count 6 digitChar
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
    dais_bks30 <- try $ string "BKS" >> count 8 digitChar >> string "30" >> T.pack <$> count 6 digitChar
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
    dais_bks39 <- try $ string "BKS" >> count 8 digitChar >> string "39" >> T.pack <$> count 6 digitChar
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
    dais_bks46 <- try $ string "BKS" >> count 8 digitChar >> string "46" >> T.pack <$> count 6 digitChar
    trnn_bks46 <- read <$> count 6 digitChar
    tdnr_bks46 <- T.pack <$> count 13 digitChar <* count 2 anySingle
    cdgt_bks46 <- read <$> count 1 digitChar
    orin_bks46 <- T.strip . T.pack <$> count 32 printChar
    enrs_bks46 <- T.strip . T.pack <$> count 58 printChar <* manyTill anySingle eol
    return BKS46 {..}

bki63Parser :: Parser BKI63
bki63Parser = do
    dais_bki63 <- try $ string "BKI" >> count 8 digitChar >> string "63" >> T.pack <$> count 6 digitChar
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


ibsHotParser :: Parser HOTIBS
ibsHotParser = do
    bhf01 <- bhfParser
    bch02 <- bch02Parser
    boh03 <- boh03Parser
    bkt06 <- bkt06Parser
    bks24s <- some bks24Parser
    bks30s <- some bks30Parser
    bks39 <- bks39Parser
    bks46s <- many bks46Parser
    bki63s <- many bki63Parser
    return HOTIBS {..}

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
    input <- TIO.readFile "./ibs-hot-sample.txt"
    parseTest ibsHotParser input
