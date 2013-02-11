object FrmProjectDrawer: TFrmProjectDrawer
  Left = 214
  Top = 179
  ClientHeight = 342
  ClientWidth = 184
  Color = clBtnFace
  Constraints.MaxWidth = 480
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    184
    342)
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TNotebook
    AlignWithMargins = True
    Left = 6
    Top = 6
    Width = 172
    Height = 330
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    TabOrder = 0
    object TPage
      Left = 0
      Top = 0
      object VtExplorer: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 172
        Height = 330
        Align = alClient
        Color = clWhite
        DefaultNodeHeight = 20
        Header.AutoSizeIndex = -1
        Header.MainColumn = -1
        Header.ParentFont = True
        HintMode = hmTooltip
        HotCursor = crHandPoint
        IncrementalSearch = isInitializedOnly
        Indent = 12
        Margin = 0
        ParentShowHint = False
        SelectionBlendFactor = 86
        ShowHint = True
        TabOrder = 0
        TextMargin = 2
        TreeOptions.AnimationOptions = [toAnimatedToggle, toAdvancedAnimatedToggle]
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toHideFocusRect, toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toHideTreeLinesIfThemed]
        TreeOptions.SelectionOptions = [toMiddleClickSelect, toMultiSelect, toRightClickSelect]
        OnBeforeCellPaint = VtExplorerBeforeCellPaint
        OnClick = VtExplorerClick
        OnCompareNodes = VtExplorerCompareNodes
        OnDblClick = VtExplorerDblClick
        OnDragAllowed = VtExplorerDragAllowed
        OnDragOver = VtExplorerDragOver
        OnDragDrop = VtExplorerDragDrop
        OnEditing = VtExplorerEditing
        OnFreeNode = VtExplorerFreeNode
        OnGetText = VtExplorerGetText
        OnPaintText = VtExplorerPaintText
        OnGetImageIndex = VtExplorerGetImageIndex
        OnGetPopupMenu = VtExplorerGetPopupMenu
        OnIncrementalSearch = VtExplorerIncrementalSearch
        OnInitChildren = VtExplorerInitChildren
        OnInitNode = VtExplorerInitNode
        OnNewText = VtExplorerNewText
        OnNodeCopying = VtExplorerNodeCopying
        OnNodeMoving = VtExplorerNodeMoving
        Columns = <>
        WideDefaultText = ''
      end
    end
  end
  object Resizer: TPanel
    Left = 0
    Top = 0
    Width = 6
    Height = 342
    Cursor = crSizeWE
    Anchors = [akLeft, akTop, akBottom]
    BevelOuter = bvNone
    FullRepaint = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    OnMouseMove = PanelMouseMove
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 48
    Top = 16
    object actProjectNew: TAction
      ShortCut = 24654
      OnExecute = actProjectNewExecute
    end
    object actProjectLoad: TAction
      ShortCut = 24655
      OnExecute = actProjectLoadExecute
    end
    object actProjectSave: TAction
      ShortCut = 16467
      OnExecute = actProjectSaveExecute
      OnUpdate = actProjectSaveUpdate
    end
    object actProjectSaveAs: TAction
      ShortCut = 49235
      OnExecute = actProjectSaveAsExecute
    end
    object actProjectAddFile: TAction
      Caption = '&Add File(s)...'
      OnExecute = actProjectAddFileExecute
    end
    object actProjectAddFolder: TAction
      Caption = 'Add &Folder'
      OnExecute = actProjectAddFolderExecute
    end
    object actProjectAddReference: TAction
      Caption = 'Add &Reference...'
      OnExecute = actProjectAddReferenceExecute
    end
    object actProjectOpen: TAction
      Caption = '&Open'
      OnExecute = actProjectOpenExecute
      OnUpdate = actProjectOpenUpdate
    end
    object actProjectBrowse: TAction
      Caption = '&Browse...'
      OnExecute = actProjectBrowseExecute
      OnUpdate = actProjectBrowseUpdate
    end
    object actProjectDelete: TAction
      Caption = '&Delete'
      ShortCut = 46
      OnExecute = actProjectDeleteExecute
      OnUpdate = actProjectDeleteUpdate
    end
    object actProjectRename: TAction
      Caption = '&Rename'
      ShortCut = 113
      OnExecute = actProjectRenameExecute
      OnUpdate = actProjectRenameUpdate
    end
    object actProjectRefresh: TAction
      Caption = '&Refresh'
      ShortCut = 116
      SecondaryShortCuts.Strings = (
        'Ctrl+R')
      OnExecute = actProjectRefreshExecute
      OnUpdate = actProjectRefreshUpdate
    end
  end
  object DlgAddFile: TOpenDialog
    Filter = 'All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofShowHelp, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofNoNetworkButton, ofEnableSizing, ofDontAddToRecent]
    Left = 16
    Top = 16
  end
  object ppmProjectDrawer: TPopupMenu
    Left = 14
    Top = 46
    object mAddFile: TMenuItem
      Action = actProjectAddFile
    end
    object mAddFolder: TMenuItem
      Action = actProjectAddFolder
    end
    object mAddReference: TMenuItem
      Action = actProjectAddReference
    end
  end
  object ppmProjectreferencedFolder: TPopupMenu
    Left = 46
    Top = 78
    object mRefBrowse: TMenuItem
      Action = actProjectBrowse
      Default = True
    end
    object spRefBrowse: TMenuItem
      Caption = '-'
    end
    object mRefDelete: TMenuItem
      Action = actProjectDelete
    end
    object mRefRename: TMenuItem
      Action = actProjectRename
    end
    object spRefRename: TMenuItem
      Caption = '-'
    end
    object mRefRefresh: TMenuItem
      Action = actProjectRefresh
    end
  end
  object ppmProjectSingle: TPopupMenu
    Left = 78
    Top = 46
    object mSingleOpen: TMenuItem
      Action = actProjectOpen
      Default = True
    end
    object mSingleBrowse: TMenuItem
      Action = actProjectBrowse
    end
    object spSingleBrowse: TMenuItem
      Caption = '-'
    end
    object mSingleDelete: TMenuItem
      Action = actProjectDelete
    end
    object mSingleRename: TMenuItem
      Action = actProjectRename
    end
  end
  object ppmProjectMultiple: TPopupMenu
    Left = 14
    Top = 78
    object mMultipleOpen: TMenuItem
      Action = actProjectOpen
      Default = True
    end
  end
  object ppmProjectVirtualFolder: TPopupMenu
    Left = 46
    Top = 46
    object mVirtualAddFile: TMenuItem
      Action = actProjectAddFile
    end
    object mVirtualAddFolder: TMenuItem
      Action = actProjectAddFolder
    end
    object mVirtualAddReference: TMenuItem
      Action = actProjectAddReference
    end
    object spVirtualAddReference: TMenuItem
      Caption = '-'
    end
    object mVirtualDelete: TMenuItem
      Action = actProjectDelete
    end
    object mVirtualRename: TMenuItem
      Action = actProjectRename
    end
  end
end
