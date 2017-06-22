module Explorer.View.Blocks
    ( blocksView
    , blockRow
    , blocksHeaderView
    , blockHeaderItemView
    , maxBlockRows
    , minBlockRows
    ) where

import Prelude

import Data.Array (length, null, slice)
import Data.DateTime (diff)
import Data.Foldable (for_)
import Data.Lens ((^.))
import Data.Monoid (mempty)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (take)
import Data.Time.Duration (Milliseconds)

import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (block, blEpochSlotNotFound, cBack2Dashboard, cLoading, cOf, common, cUnknown, cEpoch, cSlot, cAge, cTransactions, cTotalSent, cBlockLead, cSize) as I18nL
import Explorer.Lenses.State (_PageNumber, blocksViewState, blsViewPagination, blsViewPaginationEditable, currentBlocksResult, lang, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CBlockEntries, CCurrency(..), PageNumber(..), State)
import Explorer.Util.Factory (mkEpochIndex)
import Explorer.Util.String (formatADA)
import Explorer.Util.Time (prettyDuration, nominalDiffTimeToDateTime)
import Explorer.View.CSS (blocksBody, blocksBodyRow, blocksColumnAge, blocksColumnEpoch, blocksColumnLead, blocksColumnSize, blocksColumnSlot, blocksColumnTotalSent, blocksColumnTxs, blocksFailed, blocksFooter, blocksHeader) as CSS
import Explorer.View.Common (currencyCSSClass, getMaxPaginationNumber, noData, paginationView)

import Network.RemoteData (RemoteData(..), withDefault)

import Pos.Explorer.Web.ClientTypes (CBlockEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (cbeBlkHash, cbeEpoch, cbeSlot, cbeBlockLead, cbeSize, cbeTotalSent, cbeTxNum)

import Pux.DOM.HTML (HTML) as P
import Pux.DOM.Events (onClick) as P
import Pux.Renderer.React (dangerouslySetInnerHTML) as P

import Text.Smolder.HTML (a, div, span, h3, p) as S
import Text.Smolder.HTML.Attributes (className, href) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((#!), (!))

maxBlockRows :: Int
maxBlockRows = 10

minBlockRows :: Int
minBlockRows = 3

blocksView :: State -> P.HTML Action
blocksView state =
    let lang' = state ^. lang in
    S.div ! S.className "explorer-blocks"
          $ S.div ! S.className "explorer-blocks__wrapper"
                  $ S.div ! S.className "explorer-blocks__container" $ do
                        S.h3  ! S.className "headline"
                              $ S.text (( translate (I18nL.common <<< I18nL.cEpoch) lang')
                                          <> " / " <>
                                          (translate (I18nL.common <<< I18nL.cSlot) lang')
                                        )
                        case state ^. currentBlocksResult of
                            NotAsked  -> emptyBlocksView ""
                            Loading   -> emptyBlocksView $ translate (I18nL.common <<< I18nL.cLoading) lang'
                            Failure _ -> failureView lang'
                            Success blocks ->
                                let paginationViewProps =
                                        { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                                        , currentPage: state ^. (viewStates <<< blocksViewState <<< blsViewPagination)
                                        , minPage: PageNumber minPagination
                                        , maxPage: PageNumber $ getMaxPaginationNumber (length blocks) maxBlockRows
                                        , changePageAction: BlocksPaginateBlocks
                                        , editable: state ^. (viewStates <<< blocksViewState <<< blsViewPaginationEditable)
                                        , editableAction: BlocksEditBlocksPageNumber
                                        , invalidPageAction: BlocksInvalidBlocksPageNumber
                                        , disabled: false
                                        }
                                in
                                S.div do
                                    blocksHeaderView blocks lang'
                                    S.div ! S.className CSS.blocksBody
                                          $ for_ (currentBlocks state) (blockRow state)
                                    S.div ! S.className CSS.blocksFooter
                                          $ paginationView paginationViewProps

emptyBlocksView :: String -> P.HTML Action
emptyBlocksView message =
    S.div ! S.className "blocks-message"
          ! P.dangerouslySetInnerHTML message
          $ mempty

failureView :: Language -> P.HTML Action
failureView lang =
    S.div do
        S.p ! S.className CSS.blocksFailed
            $ S.text (translate (I18nL.block <<< I18nL.blEpochSlotNotFound) lang)
        S.a ! S.href (toUrl Dashboard)
            #! P.onClick (Navigate $ toUrl Dashboard)
            ! S.className "btn-back"
            $ S.text (translate (I18nL.common <<< I18nL.cBack2Dashboard) lang)

currentBlocks :: State -> CBlockEntries
currentBlocks state =
    slice minBlockIndex (minBlockIndex + maxBlockRows) blocks
    where
        blocks = withDefault [] $ state ^. currentBlocksResult
        currentBlockPage = state ^. (viewStates <<< blocksViewState <<< blsViewPagination <<< _PageNumber)
        minBlockIndex = (currentBlockPage - 1) * maxBlockRows

blockRow :: State -> CBlockEntry -> P.HTML Action
blockRow state (CBlockEntry entry) =
    S.div ! S.className CSS.blocksBodyRow $ do
        blockColumn { label: show $ entry ^. cbeEpoch
                    , mRoute: Just <<< Epoch <<< mkEpochIndex $ entry ^. cbeEpoch
                    , clazz: CSS.blocksColumnEpoch
                    , mCurrency: Nothing
                    }
        blockColumn { label: show $ entry ^. cbeSlot
                    , mRoute: Just <<< Block $ entry ^. cbeBlkHash
                    , clazz: CSS.blocksColumnSlot
                    , mCurrency: Nothing
                    }
        blockColumn { label: labelAge
                    , mRoute: Nothing
                    , clazz: CSS.blocksColumnAge
                    , mCurrency: Nothing
                    }
        blockColumn { label: show $ entry ^. cbeTxNum
                    , mRoute: Nothing
                    , clazz: CSS.blocksColumnTxs
                    , mCurrency: Nothing
                    }
        blockColumn { label: formatADA (entry ^. cbeTotalSent) $ state ^. lang
                    , mRoute: Nothing
                    , clazz: CSS.blocksColumnTotalSent
                    , mCurrency: Just ADA
                    }
        blockColumn { label: labelBlockLead
                    , mRoute: Nothing
                    , clazz: CSS.blocksColumnLead
                    , mCurrency: Nothing
                    }
        blockColumn { label: show $ entry ^. cbeSize
                    , mRoute: Nothing
                    , clazz: CSS.blocksColumnSize
                    , mCurrency: Nothing
                    }
    where
        language = state ^. lang
        labelAge = fromMaybe noData $ (prettyDuration language :: Milliseconds -> String) <<< diff state.now <$> (nominalDiffTimeToDateTime  =<< entry.cbeTimeIssued)
        labelBlockLead = fromMaybe (translate (I18nL.common <<< I18nL.cUnknown) language)
                            $ take 7 <$> (entry ^. cbeBlockLead)


type BlockColumnProps =
    { label :: String
    , clazz :: String
    , mCurrency :: Maybe CCurrency
    , mRoute :: Maybe Route
    }

blockColumn :: BlockColumnProps -> P.HTML Action
blockColumn props =
    let tag = case props.mRoute of
                  Just route ->
                      S.a ! S.href (toUrl route)
                          #! P.onClick (Navigate $ toUrl route)
                  Nothing ->
                      S.div
    in
    tag ! S.className props.clazz
        $ if isJust props.mCurrency
              then S.span ! S.className (currencyCSSClass props.mCurrency)
                          $ S.text props.label
              else S.text props.label

type BlocksHeaderProps =
    { label :: String
    , clazz :: String
    }

mkBlocksHeaderProps :: Language -> Array BlocksHeaderProps
mkBlocksHeaderProps lang =
    [ { label: translate (I18nL.common <<< I18nL.cEpoch) lang
      , clazz: CSS.blocksColumnEpoch
      }
    , { label: translate (I18nL.common <<< I18nL.cSlot) lang
      , clazz: CSS.blocksColumnSlot
      }
    , { label: translate (I18nL.common <<< I18nL.cAge) lang
      , clazz: CSS.blocksColumnAge
      }
    , { label: translate (I18nL.common <<< I18nL.cTransactions) lang
      , clazz: CSS.blocksColumnTxs
      }
    , { label: translate (I18nL.common <<< I18nL.cTotalSent) lang
      , clazz: CSS.blocksColumnTotalSent
      }
    , { label: translate (I18nL.common <<< I18nL.cBlockLead) lang
      , clazz: CSS.blocksColumnLead
      }
    , { label: translate (I18nL.common <<< I18nL.cSize) lang
      , clazz: CSS.blocksColumnSize
      }
    ]

blocksHeaderView :: CBlockEntries -> Language -> P.HTML Action
blocksHeaderView blocks lang =
    S.div ! S.className (CSS.blocksHeader <> if null blocks then " hide" else "")
          $ for_ (mkBlocksHeaderProps lang) blockHeaderItemView

blockHeaderItemView :: BlocksHeaderProps -> P.HTML Action
blockHeaderItemView props =
    S.div ! S.className props.clazz
          $ S.text props.label
