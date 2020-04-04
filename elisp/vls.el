(defun my-lsp-set-cfg (workspace)
  "Set lsp configuration."
  (let ((lsp-cfg `(:vetur
                   (:completion
                    (:autoImport t :useScaffoldSnippets t :tagCasing "kebab")
                    :grammar
                    (:customBlocks
                     (:docs "md" :i18n "json"))
                    :validation
                    (:template t :style t :script t)
                    :format
                    (:options
                     (:tabSize 2 :useTabs :json-false)
                     :defaultFormatter
                     (:html "prettyhtml" :css "prettier" :postcss "prettier" :scss "prettier" :less "prettier" :stylus "stylus-supremacy" :js "prettier-eslint" :ts "prettier")
                     :defaultFormatterOptions
                     (:js-beautify-html
                      (:wrap_attributes "force-expand-multiline")
                      :prettyhtml
                      (:printWidth 100 :singleQuote :json-false :wrapAttributes :json-false :sortAttributes :json-false))
                     :styleInitialIndent :json-false :scriptInitialIndent :json-false)
                    :trace
                    (:server "verbose")
                    :dev
                    (:vlsPath ""))
                   :emmet
                   (:showExpandedAbbreviation "always" :showAbbreviationSuggestions t :includeLanguages nil :variables nil :syntaxProfiles nil :excludeLanguages
                                              ["markdown"]
                                              :extensionsPath nil :triggerExpansionOnTab :json-false :preferences nil :showSuggestionsAsSnippets :json-false :optimizeStylesheetParsing t)
                   :html
                   (:experimental
                    (:custom
                     (:tags
                      []
                      :attributes
                      []))
                    :format
                    (:enable t :wrapLineLength 120 :unformatted "wbr" :contentUnformatted "pre,code,textarea" :indentInnerHtml :json-false :preserveNewLines t :maxPreserveNewLines nil :indentHandlebars :json-false :endWithNewline :json-false :extraLiners "head, body, /html" :wrapAttributes "auto")
                    :suggest
                    (:angular1 :json-false :ionic :json-false :html5 t)
                    :validate
                    (:scripts t :styles t)
                    :autoClosingTags t :trace
                    (:server "off"))
                   :javascript
                   (:referencesCodeLens
                    (:enabled :json-false)
                    :suggest
                    (:completeFunctionCalls :json-false :names t :paths t :autoImports t :enabled t)
                    :validate
                    (:enable t)
                    :format
                    (:enable t :insertSpaceAfterCommaDelimiter t :insertSpaceAfterConstructor :json-false :insertSpaceAfterSemicolonInForStatements t :insertSpaceBeforeAndAfterBinaryOperators t :insertSpaceAfterKeywordsInControlFlowStatements t :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :insertSpaceBeforeFunctionParenthesis :json-false :insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis :json-false :insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets :json-false :insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces t :insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces :json-false :insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces :json-false :placeOpenBraceOnNewLineForFunctions :json-false :placeOpenBraceOnNewLineForControlBlocks :json-false)
                    :implicitProjectConfig
                    (:checkJs :json-false :experimentalDecorators :json-false)
                    :suggestionActions
                    (:enabled t)
                    :preferences
                    (:quoteStyle "auto" :importModuleSpecifier "auto")
                    :updateImportsOnFileMove
                    (:enabled "prompt")
                    :autoClosingTags t)
                   :typescript
                   (:tsdk nil :disableAutomaticTypeAcquisition :json-false :npm nil :check
                          (:npmIsInstalled t)
                          :referencesCodeLens
                          (:enabled :json-false)
                          :implementationsCodeLens
                          (:enabled :json-false)
                          :tsserver
                          (:log "verbose" :pluginPaths
                                []
                                :trace "messages")
                          :suggest
                          (:completeFunctionCalls :json-false :paths t :autoImports t :enabled t)
                          :reportStyleChecksAsWarnings t :validate
                          (:enable t)
                          :format
                          (:enable t :insertSpaceAfterCommaDelimiter t :insertSpaceAfterConstructor :json-false :insertSpaceAfterSemicolonInForStatements t :insertSpaceBeforeAndAfterBinaryOperators t :insertSpaceAfterKeywordsInControlFlowStatements t :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :insertSpaceBeforeFunctionParenthesis :json-false :insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis :json-false :insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets :json-false :insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces t :insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces :json-false :insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces :json-false :insertSpaceAfterTypeAssertion :json-false :placeOpenBraceOnNewLineForFunctions :json-false :placeOpenBraceOnNewLineForControlBlocks :json-false)
                          :tsc
                          (:autoDetect "on")
                          :locale nil :suggestionActions
                          (:enabled t)
                          :preferences
                          (:quoteStyle "auto" :importModuleSpecifier "auto")
                          :updateImportsOnFileMove
                          (:enabled "prompt")
                          :autoClosingTags t :surveys
                          (:enabled t))
                   :prettier (:trailingComma "all") :stylusSupremacy nil)
                 ))
    (lsp--set-configuration lsp-cfg)))
(setq spacemacs-jump-handlers-vue-html-mode nil)
(setq spacemacs-jump-handlers-vue-mode nil)
(add-to-list 'lsp-language-id-configuration '(vue-html-mode . "vue"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "~/.vscode/extensions/octref.vetur-0.17.0/server/bin/vls")
                  :major-modes '(vue-html-mode vue-mode web-mode)
                  :multi-root t
                  :ignore-messages '("readFile .*? requested by Vue but content not available")
                  :initialization-options (lambda ()
                                            '(:editor
                                              (:fontFamily "'Droid Sans Mono', 'monospace', monospace, 'Droid Sans Fallback'" :fontWeight "normal" :fontSize 14 :lineHeight 0 :letterSpacing 0 :lineNumbers "on" :rulers
                                                           []
                                                           :wordSeparators "`~!@#$%^&*()-=+[{]}\\|;:'\",.<>/?" :tabSize 4 :insertSpaces t :detectIndentation t :roundedSelection t :scrollBeyondLastLine t :scrollBeyondLastColumn 5 :smoothScrolling :json-false :minimap
                                                           (:enabled t :side "right" :showSlider "mouseover" :renderCharacters t :maxColumn 120)
                                                           :hover
                                                           (:enabled t :delay 300 :sticky t)
                                                           :find
                                                           (:seedSearchStringFromSelection t :autoFindInSelection :json-false)
                                                           :wordWrap "off" :wordWrapColumn 80 :wrappingIndent "same" :mouseWheelScrollSensitivity 1 :multiCursorModifier "alt" :multiCursorMergeOverlapping t :quickSuggestions
                                                           (:other t :comments :json-false :strings :json-false)
                                                           :quickSuggestionsDelay 10 :parameterHints
                                                           (:enabled t :cycle :json-false)
                                                           :autoClosingBrackets "languageDefined" :autoClosingQuotes "languageDefined" :autoSurround "languageDefined" :formatOnType :json-false :formatOnPaste :json-false :autoIndent t :suggestOnTriggerCharacters t :acceptSuggestionOnEnter "on" :acceptSuggestionOnCommitCharacter t :snippetSuggestions "inline" :emptySelectionClipboard t :copyWithSyntaxHighlighting t :wordBasedSuggestions t :suggestSelection "recentlyUsed" :suggestFontSize 0 :suggestLineHeight 0 :tabCompletion "off" :suggest
                                                           (:filterGraceful t :localityBonus :json-false :snippetsPreventQuickSuggestions t)
                                                           :selectionHighlight t :occurrencesHighlight t :overviewRulerLanes 3 :overviewRulerBorder t :cursorBlinking "blink" :mouseWheelZoom :json-false :cursorSmoothCaretAnimation :json-false :cursorStyle "line" :cursorWidth 0 :fontLigatures :json-false :hideCursorInOverviewRuler :json-false :renderWhitespace "none" :renderControlCharacters :json-false :renderIndentGuides t :highlightActiveIndentGuide t :renderLineHighlight "line" :codeLens t :folding t :foldingStrategy "auto" :showFoldingControls "mouseover" :matchBrackets t :glyphMargin t :useTabStops t :trimAutoWhitespace t :stablePeek :json-false :dragAndDrop t :accessibilitySupport "auto" :showUnused t :links t :colorDecorators t :lightbulb
                                                           (:enabled t)
                                                           :codeActionsOnSave nil :codeActionsOnSaveTimeout 750 :selectionClipboard t :largeFileOptimizations t :tokenColorCustomizations nil :formatOnSave :json-false :formatOnSaveTimeout 750)
                                              :diffEditor
                                              (:renderSideBySide t :ignoreTrimWhitespace t :renderIndicators t)
                                              :http
                                              (:proxy "" :proxyStrictSSL t :proxyAuthorization nil :proxySupport "off")
                                              :telemetry
                                              (:enableTelemetry t :enableCrashReporter t)
                                              :workbench
                                              (:list
                                               (:multiSelectModifier "ctrlCmd" :openMode "singleClick")
                                               :tree
                                               (:horizontalScrolling :json-false)
                                               :statusBar
                                               (:feedback
                                                (:visible t)
                                                :visible t)
                                               :colorTheme "Default Dark+" :iconTheme "vs-seti" :colorCustomizations nil :editor
                                               (:showTabs t :highlightModifiedTabs :json-false :labelFormat "default" :tabCloseButton "right" :tabSizing "fit" :showIcons t :enablePreview t :enablePreviewFromQuickOpen t :closeOnFileDelete :json-false :openPositioning "right" :openSideBySideDirection "right" :closeEmptyGroups t :revealIfOpen :json-false :restoreViewState t :centeredLayoutAutoResize t)
                                               :commandPalette
                                               (:history 50 :preserveInput :json-false)
                                               :quickOpen
                                               (:closeOnFocusLost t :preserveInput :json-false)
                                               :settings
                                               (:openDefaultSettings t :openDefaultKeybindings t :enableNaturalLanguageSearch t :settingsSearchTocBehavior "filter" :editor "ui")
                                               :sideBar
                                               (:location "left")
                                               :panel
                                               (:defaultLocation "bottom")
                                               :activityBar
                                               (:visible t)
                                               :view
                                               (:alwaysShowHeaderActions :json-false)
                                               :enableExperiments t :enableLegacyStorage :json-false :tips
                                               (:enabled t)
                                               :startupEditor "welcomePage")
                                              :breadcrumbs
                                              (:enabled :json-false :filePath "on" :symbolPath "on" :symbolSortOrder "position")
                                              :problems
                                              (:decorations
                                               (:enabled t)
                                               :autoReveal t)
                                              :scm
                                              (:alwaysShowProviders :json-false :diffDecorations "all" :diffDecorationsGutterWidth 3 :alwaysShowActions :json-false)
                                              :outline
                                              (:icons t :problems
                                                      (:enabled t :colors t :badges t))
                                              :files
                                              (:exclude
                                               (:**/\.git t :**/\.svn t :**/\.hg t :**/CVS t :**/\.DS_Store t :**/\.classpath t :**/\.project t :**/\.settings t :**/\.factorypath t)
                                               :associations nil :encoding "utf8" :autoGuessEncoding :json-false :eol "auto" :enableTrash t :trimTrailingWhitespace :json-false :insertFinalNewline :json-false :trimFinalNewlines :json-false :autoSave "off" :autoSaveDelay 1000 :watcherExclude
                                               (:**/\.git/objects/** t :**/\.git/subtree-cache/** t :**/node_modules/** t)
                                               :hotExit "onExit" :useExperimentalFileWatcher :json-false :defaultLanguage "" :maxMemoryForLargeFilesMB 4096)
                                              :explorer
                                              (:openEditors
                                               (:visible 9)
                                               :autoReveal t :enableDragAndDrop t :confirmDragAndDrop t :confirmDelete t :sortOrder "default" :decorations
                                               (:colors t :badges t))
                                              :search
                                              (:exclude
                                               (:**/node_modules t :**/bower_components t)
                                               :useRipgrep t :useLegacySearch :json-false :useIgnoreFiles t :useGlobalIgnoreFiles :json-false :quickOpen
                                               (:includeSymbols :json-false :includeHistory t)
                                               :followSymlinks t :smartCase :json-false :location "sidebar" :collapseResults "auto" :useReplacePreview t :showLineNumbers :json-false :runInExtensionHost :json-false :usePCRE2 :json-false :actionsPosition "auto")
                                              :searchRipgrep
                                              (:enable :json-false)
                                              :window
                                              (:openFilesInNewWindow "off" :openFoldersInNewWindow "default" :openWithoutArgumentsInNewWindow "on" :restoreWindows "one" :restoreFullscreen :json-false :zoomLevel 0 :title "${dirty}${activeEditorShort}${separator}${rootName}${separator}${appName}" :newWindowDimensions "default" :closeWhenEmpty :json-false :menuBarVisibility "default" :enableMenuBarMnemonics t :titleBarStyle "custom")
                                              :zenMode
                                              (:fullScreen t :centerLayout t :hideTabs t :hideStatusBar t :hideActivityBar t :restore :json-false)
                                              :debug
                                              (:allowBreakpointsEverywhere :json-false :openExplorerOnEnd :json-false :inlineValues t :toolBarLocation "floating" :showInStatusBar "onFirstSessionStart" :internalConsoleOptions "openOnFirstSessionStart" :openDebug "openOnSessionStart" :enableAllHovers t :node
                                                                           (:autoAttach "disabled"))
                                              :launch
                                              (:configurations
                                               []
                                               :compounds
                                               [])
                                              :terminal
                                              (:integrated
                                               (:shell
                                                (:linux "/usr/bin/zsh" :osx "/bin/bash" :windows "cmd.exe")
                                                :shellArgs
                                                (:linux
                                                 []
                                                 :osx
                                                 ["-l"]
                                                 :windows
                                                 [])
                                                :macOptionIsMeta :json-false :macOptionClickForcesSelection :json-false :copyOnSelection :json-false :drawBoldTextInBrightColors t :fontFamily "" :fontSize 14 :letterSpacing 0 :lineHeight 1 :fontWeight "normal" :fontWeightBold "bold" :cursorBlinking :json-false :cursorStyle "block" :scrollback 1000 :setLocaleVariables :json-false :rendererType "auto" :rightClickBehavior "default" :cwd "" :confirmOnExit :json-false :enableBell :json-false :commandsToSkipShell
                                                ["editor.action.toggleTabFocusMode" "workbench.action.debug.continue" "workbench.action.debug.pause" "workbench.action.debug.restart" "workbench.action.debug.run" "workbench.action.debug.start" "workbench.action.debug.stepInto" "workbench.action.debug.stepOut" "workbench.action.debug.stepOver" "workbench.action.debug.stop" "workbench.action.firstEditorInGroup" "workbench.action.focusActiveEditorGroup" "workbench.action.focusEighthEditorGroup" "workbench.action.focusFifthEditorGroup" "workbench.action.focusFirstEditorGroup" "workbench.action.focusFourthEditorGroup" "workbench.action.focusLastEditorGroup" "workbench.action.focusSecondEditorGroup" "workbench.action.focusSeventhEditorGroup" "workbench.action.focusSixthEditorGroup" "workbench.action.focusThirdEditorGroup" "workbench.action.lastEditorInGroup" "workbench.action.navigateDown" "workbench.action.navigateLeft" "workbench.action.navigateRight" "workbench.action.navigateUp" "workbench.action.nextPanelView" "workbench.action.nextSideBarView" "workbench.action.openNextRecentlyUsedEditorInGroup" "workbench.action.openPreviousRecentlyUsedEditorInGroup" "workbench.action.previousPanelView" "workbench.action.previousSideBarView" "workbench.action.quickOpen" "workbench.action.quickOpenPreviousEditor" "workbench.action.quickOpenView" "workbench.action.showCommands" "workbench.action.tasks.build" "workbench.action.tasks.reRunTask" "workbench.action.tasks.restartTask" "workbench.action.tasks.runTask" "workbench.action.tasks.showLog" "workbench.action.tasks.showTasks" "workbench.action.tasks.terminate" "workbench.action.tasks.test" "workbench.action.terminal.clear" "workbench.action.terminal.clearSelection" "workbench.action.terminal.copySelection" "workbench.action.terminal.deleteToLineStart" "workbench.action.terminal.deleteWordLeft" "workbench.action.terminal.deleteWordRight" "workbench.action.terminal.findNextTerminalFocus" "workbench.action.terminal.findPreviousTerminalFocus" "workbench.action.terminal.focus" "workbench.action.terminal.focusAtIndex1" "workbench.action.terminal.focusAtIndex2" "workbench.action.terminal.focusAtIndex3" "workbench.action.terminal.focusAtIndex4" "workbench.action.terminal.focusAtIndex5" "workbench.action.terminal.focusAtIndex6" "workbench.action.terminal.focusAtIndex7" "workbench.action.terminal.focusAtIndex8" "workbench.action.terminal.focusAtIndex9" "workbench.action.terminal.focusFindWidget" "workbench.action.terminal.focusNext" "workbench.action.terminal.focusNextPane" "workbench.action.terminal.focusPrevious" "workbench.action.terminal.focusPreviousPane" "workbench.action.terminal.hideFindWidget" "workbench.action.terminal.kill" "workbench.action.terminal.moveToLineEnd" "workbench.action.terminal.moveToLineStart" "workbench.action.terminal.new" "workbench.action.terminal.newInActiveWorkspace" "workbench.action.terminal.paste" "workbench.action.terminal.resizePaneDown" "workbench.action.terminal.resizePaneLeft" "workbench.action.terminal.resizePaneRight" "workbench.action.terminal.resizePaneUp" "workbench.action.terminal.runActiveFile" "workbench.action.terminal.runSelectedText" "workbench.action.terminal.scrollDown" "workbench.action.terminal.scrollDownPage" "workbench.action.terminal.scrollToBottom" "workbench.action.terminal.scrollToNextCommand" "workbench.action.terminal.scrollToPreviousCommand" "workbench.action.terminal.scrollToTop" "workbench.action.terminal.scrollUp" "workbench.action.terminal.scrollUpPage" "workbench.action.terminal.selectAll" "workbench.action.terminal.selectToNextCommand" "workbench.action.terminal.selectToNextLine" "workbench.action.terminal.selectToPreviousCommand" "workbench.action.terminal.selectToPreviousLine" "workbench.action.terminal.sendSequence" "workbench.action.terminal.split" "workbench.action.terminal.splitInActiveWorkspace" "workbench.action.terminal.toggleFindCaseSensitiveTerminalFocus" "workbench.action.terminal.toggleFindRegexTerminalFocus" "workbench.action.terminal.toggleFindWholeWordTerminalFocus" "workbench.action.terminal.toggleTerminal" "workbench.action.toggleFullScreen" "workbench.action.toggleMaximizedPanel" "workbench.action.togglePanel"]
                                                :env
                                                (:osx nil :linux nil :windows nil)
                                                :showExitAlert t :experimentalBufferImpl "JsArray" :splitCwd "inherited")
                                               :explorerKind "integrated" :external
                                               (:windowsExec "C:\\Windows\\System32\\cmd.exe" :osxExec "Terminal.app" :linuxExec "x-terminal-emulator"))
                                              :keyboard
                                              (:dispatch "code")
                                              :extensions
                                              (:autoUpdate t :autoCheckUpdates t :ignoreRecommendations :json-false :showRecommendationsOnlyOnDemand :json-false :closeExtensionDetailsOnViewChange :json-false)
                                              :css
                                              (:validate t :colorDecorators
                                                         (:enable t)
                                                         :lint
                                                         (:compatibleVendorPrefixes "ignore" :vendorPrefix "warning" :duplicateProperties "ignore" :emptyRules "warning" :importStatement "ignore" :boxModel "ignore" :universalSelector "ignore" :zeroUnits "ignore" :fontFaceProperties "warning" :hexColorLength "error" :argumentsInColorFunction "error" :unknownProperties "warning" :validProperties
                                                                                    []
                                                                                    :ieHack "ignore" :unknownVendorSpecificProperties "ignore" :propertyIgnoredDueToDisplay "warning" :important "ignore" :float "ignore" :idSelector "ignore" :unknownAtRules "warning")
                                                         :trace
                                                         (:server "off"))
                                              :scss
                                              (:validate t :colorDecorators
                                                         (:enable t)
                                                         :lint
                                                         (:compatibleVendorPrefixes "ignore" :vendorPrefix "warning" :duplicateProperties "ignore" :emptyRules "warning" :importStatement "ignore" :boxModel "ignore" :universalSelector "ignore" :zeroUnits "ignore" :fontFaceProperties "warning" :hexColorLength "error" :argumentsInColorFunction "error" :unknownProperties "warning" :validProperties
                                                                                    []
                                                                                    :ieHack "ignore" :unknownVendorSpecificProperties "ignore" :propertyIgnoredDueToDisplay "warning" :important "ignore" :float "ignore" :idSelector "ignore"))
                                              :less
                                              (:validate t :colorDecorators
                                                         (:enable t)
                                                         :lint
                                                         (:compatibleVendorPrefixes "ignore" :vendorPrefix "warning" :duplicateProperties "ignore" :emptyRules "warning" :importStatement "ignore" :boxModel "ignore" :universalSelector "ignore" :zeroUnits "ignore" :fontFaceProperties "warning" :hexColorLength "error" :argumentsInColorFunction "error" :unknownProperties "warning" :validProperties
                                                                                    []
                                                                                    :ieHack "ignore" :unknownVendorSpecificProperties "ignore" :propertyIgnoredDueToDisplay "warning" :important "ignore" :float "ignore" :idSelector "ignore"))
                                              :emmet
                                              (:showExpandedAbbreviation "always" :showAbbreviationSuggestions t :includeLanguages nil :variables nil :syntaxProfiles nil :excludeLanguages
                                                                         ["markdown"]
                                                                         :extensionsPath nil :triggerExpansionOnTab :json-false :preferences nil :showSuggestionsAsSnippets :json-false :optimizeStylesheetParsing t)
                                              :git
                                              (:enabled t :path nil :autoRepositoryDetection t :autorefresh t :autofetch :json-false :branchValidationRegex "" :branchWhitespaceChar "-" :confirmSync t :countBadge "all" :checkoutType "all" :ignoreLegacyWarning t :ignoreMissingGitWarning :json-false :ignoreLimitWarning :json-false :defaultCloneDirectory nil :enableSmartCommit :json-false :enableCommitSigning :json-false :confirmEmptyCommits t :decorations
                                                        (:enabled t)
                                                        :promptToSaveFilesBeforeCommit t :postCommitCommand "none" :showInlineOpenFileAction t :showPushSuccessNotification :json-false :inputValidation "warn" :inputValidationLength 72 :detectSubmodules t :detectSubmodulesLimit 10 :alwaysShowStagedChangesResourceGroup :json-false :alwaysSignOff :json-false :ignoredRepositories
                                                        []
                                                        :scanRepositories
                                                        []
                                                        :showProgress t :rebaseWhenSync :json-false :fetchOnPull :json-false :allowForcePush :json-false :useForcePushWithLease t :confirmForcePush t :openDiffOnClick t)
                                              :grunt
                                              (:autoDetect "on")
                                              :gulp
                                              (:autoDetect "on")
                                              :html
                                              (:experimental
                                               (:custom
                                                (:tags
                                                 []
                                                 :attributes
                                                 []))
                                               :format
                                               (:enable t :wrapLineLength 120 :unformatted "wbr" :contentUnformatted "pre,code,textarea" :indentInnerHtml :json-false :preserveNewLines t :maxPreserveNewLines nil :indentHandlebars :json-false :endWithNewline :json-false :extraLiners "head, body, /html" :wrapAttributes "auto")
                                               :suggest
                                               (:angular1 :json-false :ionic :json-false :html5 t)
                                               :validate
                                               (:scripts t :styles t)
                                               :autoClosingTags t :trace
                                               (:server "off"))
                                              :jake
                                              (:autoDetect "on")
                                              :json
                                              (:schemas
                                               []
                                               :format
                                               (:enable t)
                                               :trace
                                               (:server "off")
                                               :colorDecorators
                                               (:enable t))
                                              :markdown
                                              (:styles
                                               []
                                               :previewFrontMatter "hide" :preview
                                               (:breaks :json-false :linkify t :fontFamily "-apple-system, BlinkMacSystemFont, 'Segoe WPC', 'Segoe UI', 'HelveticaNeue-Light', 'Ubuntu', 'Droid Sans', sans-serif" :fontSize 14 :lineHeight 1.6 :scrollPreviewWithEditor t :scrollPreviewWithEditorSelection t :markEditorSelection t :scrollEditorWithPreview t :doubleClickToSwitchToEditor t :openMarkdownLinks "inPreview")
                                               :trace "off")
                                              :merge-conflict
                                              (:codeLens
                                               (:enabled t)
                                               :decorators
                                               (:enabled t)
                                               :autoNavigateNextConflict
                                               (:enabled :json-false))
                                              :npm
                                              (:autoDetect "on" :runSilent :json-false :packageManager "npm" :exclude "" :enableScriptExplorer :json-false :scriptExplorerAction "open" :fetchOnlinePackageInfo t)
                                              :php
                                              (:suggest
                                               (:basic t)
                                               :validate
                                               (:enable t :executablePath nil :run "onSave"))
                                              :typescript
                                              (:tsdk nil :disableAutomaticTypeAcquisition :json-false :npm nil :check
                                                     (:npmIsInstalled t)
                                                     :referencesCodeLens
                                                     (:enabled :json-false)
                                                     :implementationsCodeLens
                                                     (:enabled :json-false)
                                                     :tsserver
                                                     (:log "verbose" :pluginPaths
                                                           []
                                                           :trace "messages")
                                                     :suggest
                                                     (:completeFunctionCalls :json-false :paths t :autoImports t :enabled t)
                                                     :reportStyleChecksAsWarnings t :validate
                                                     (:enable t)
                                                     :format
                                                     (:enable t :insertSpaceAfterCommaDelimiter t :insertSpaceAfterConstructor :json-false :insertSpaceAfterSemicolonInForStatements t :insertSpaceBeforeAndAfterBinaryOperators t :insertSpaceAfterKeywordsInControlFlowStatements t :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :insertSpaceBeforeFunctionParenthesis :json-false :insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis :json-false :insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets :json-false :insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces t :insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces :json-false :insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces :json-false :insertSpaceAfterTypeAssertion :json-false :placeOpenBraceOnNewLineForFunctions :json-false :placeOpenBraceOnNewLineForControlBlocks :json-false)
                                                     :tsc
                                                     (:autoDetect "on")
                                                     :locale nil :suggestionActions
                                                     (:enabled t)
                                                     :preferences
                                                     (:quoteStyle "auto" :importModuleSpecifier "auto")
                                                     :updateImportsOnFileMove
                                                     (:enabled "prompt")
                                                     :autoClosingTags t :surveys
                                                     (:enabled t))
                                              :javascript
                                              (:referencesCodeLens
                                               (:enabled :json-false)
                                               :suggest
                                               (:completeFunctionCalls :json-false :names t :paths t :autoImports t :enabled t)
                                               :validate
                                               (:enable t)
                                               :format
                                               (:enable t :insertSpaceAfterCommaDelimiter t :insertSpaceAfterConstructor :json-false :insertSpaceAfterSemicolonInForStatements t :insertSpaceBeforeAndAfterBinaryOperators t :insertSpaceAfterKeywordsInControlFlowStatements t :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :insertSpaceBeforeFunctionParenthesis :json-false :insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis :json-false :insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets :json-false :insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces t :insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces :json-false :insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces :json-false :placeOpenBraceOnNewLineForFunctions :json-false :placeOpenBraceOnNewLineForControlBlocks :json-false)
                                               :implicitProjectConfig
                                               (:checkJs :json-false :experimentalDecorators :json-false)
                                               :suggestionActions
                                               (:enabled t)
                                               :preferences
                                               (:quoteStyle "auto" :importModuleSpecifier "auto")
                                               :updateImportsOnFileMove
                                               (:enabled "prompt")
                                               :autoClosingTags t)
                                              :jsDocCompletion
                                              (:enabled t)
                                              :ccls
                                              (:launch
                                               (:command "ccls" :args
                                                         [])
                                               :cache
                                               (:directory ".ccls-cache" :hierarchicalPath :json-false)
                                               :highlighting
                                               (:enabled
                                                (:types :json-false :freeStandingFunctions :json-false :memberFunctions :json-false :freeStandingVariables :json-false :memberVariables :json-false :namespaces :json-false :macros :json-false :enums :json-false :typeAliases :json-false :enumConstants :json-false :staticMemberFunctions :json-false :parameters :json-false :templateParameters :json-false :staticMemberVariables :json-false :globalVariables :json-false)
                                                :colors
                                                (:types
                                                 ["#e1afc3" "#d533bb" "#9b677f" "#e350b6" "#a04360" "#dd82bc" "#de3864" "#ad3f87" "#dd7a90" "#e0438a"]
                                                 :freeStandingFunctions
                                                 ["#e5b124" "#927754" "#eb992c" "#e2bf8f" "#d67c17" "#88651e" "#e4b953" "#a36526" "#b28927" "#d69855"]
                                                 :memberFunctions
                                                 ["#e5b124" "#927754" "#eb992c" "#e2bf8f" "#d67c17" "#88651e" "#e4b953" "#a36526" "#b28927" "#d69855"]
                                                 :freeStandingVariables
                                                 ["#587d87" "#26cdca" "#397797" "#57c2cc" "#306b72" "#6cbcdf" "#368896" "#3ea0d2" "#48a5af" "#7ca6b7"]
                                                 :memberVariables
                                                 ["#587d87" "#26cdca" "#397797" "#57c2cc" "#306b72" "#6cbcdf" "#368896" "#3ea0d2" "#48a5af" "#7ca6b7"]
                                                 :namespaces
                                                 ["#429921" "#58c1a4" "#5ec648" "#36815b" "#83c65d" "#417b2f" "#43cc71" "#7eb769" "#58bf89" "#3e9f4a"]
                                                 :macros
                                                 ["#e79528" "#c5373d" "#e8a272" "#d84f2b" "#a67245" "#e27a33" "#9b4a31" "#b66a1e" "#e27a71" "#cf6d49"]
                                                 :enums
                                                 ["#e1afc3" "#d533bb" "#9b677f" "#e350b6" "#a04360" "#dd82bc" "#de3864" "#ad3f87" "#dd7a90" "#e0438a"]
                                                 :typeAliases
                                                 ["#e1afc3" "#d533bb" "#9b677f" "#e350b6" "#a04360" "#dd82bc" "#de3864" "#ad3f87" "#dd7a90" "#e0438a"]
                                                 :staticMemberFunctions
                                                 ["#e5b124" "#927754" "#eb992c" "#e2bf8f" "#d67c17" "#88651e" "#e4b953" "#a36526" "#b28927" "#d69855"]
                                                 :enumConstants
                                                 ["#587d87" "#26cdca" "#397797" "#57c2cc" "#306b72" "#6cbcdf" "#368896" "#3ea0d2" "#48a5af" "#7ca6b7"]
                                                 :parameters
                                                 ["#587d87" "#26cdca" "#397797" "#57c2cc" "#306b72" "#6cbcdf" "#368896" "#3ea0d2" "#48a5af" "#7ca6b7"]
                                                 :templateParameters
                                                 ["#e1afc3" "#d533bb" "#9b677f" "#e350b6" "#a04360" "#dd82bc" "#de3864" "#ad3f87" "#dd7a90" "#e0438a"]
                                                 :staticMemberVariables
                                                 ["#587d87" "#26cdca" "#397797" "#57c2cc" "#306b72" "#6cbcdf" "#368896" "#3ea0d2" "#48a5af" "#7ca6b7"]
                                                 :globalVariables
                                                 ["#587d87" "#26cdca" "#397797" "#57c2cc" "#306b72" "#6cbcdf" "#368896" "#3ea0d2" "#48a5af" "#7ca6b7"])
                                                :underline
                                                (:types :json-false :freeStandingFunctions :json-false :memberFunctions :json-false :freeStandingVariables :json-false :memberVariables :json-false :namespaces :json-false :macros :json-false :enums :json-false :typeAliases :json-false :enumConstants :json-false :staticMemberFunctions t :parameters :json-false :templateParameters :json-false :staticMemberVariables t :globalVariables :json-false)
                                                :italic
                                                (:types :json-false :freeStandingFunctions :json-false :memberFunctions t :freeStandingVariables :json-false :memberVariables t :namespaces :json-false :macros :json-false :enums :json-false :typeAliases :json-false :enumConstants :json-false :staticMemberFunctions :json-false :parameters t :templateParameters :json-false :staticMemberVariables :json-false :globalVariables :json-false)
                                                :bold
                                                (:types t :freeStandingFunctions :json-false :memberFunctions :json-false :freeStandingVariables :json-false :memberVariables :json-false :namespaces t :macros :json-false :enums t :typeAliases t :enumConstants t :staticMemberFunctions :json-false :parameters :json-false :templateParameters t :staticMemberVariables :json-false :globalVariables :json-false))
                                               :clang
                                               (:extraArgs
                                                []
                                                :excludeArgs
                                                []
                                                :pathMappings
                                                []
                                                :resourceDir "")
                                               :index
                                               (:whitelist
                                                []
                                                :blacklist
                                                []
                                                :initialBlacklist
                                                []
                                                :initialWhitelist
                                                []
                                                :multiVersion 0 :onChange :json-false :threads 0 :trackDependency 2)
                                               :misc
                                               (:compilationDatabaseCommand "" :compilationDatabaseDirectory "" :showInactiveRegions t)
                                               :completion
                                               (:include
                                                (:maxPathSize 37 :suffixWhitelist
                                                              [".h" ".hpp" ".hh"]
                                                              :whitelist
                                                              []
                                                              :blacklist
                                                              [])
                                                :caseSensitivity 2 :detailedLabel :json-false :duplicateOptional :json-false :enableSnippetInsertion :json-false)
                                               :diagnostics
                                               (:blacklist
                                                []
                                                :whitelist
                                                []
                                                :onChange 1000 :onOpen 0 :onSave 0 :spellChecking t)
                                               :highlight
                                               (:blacklist nil :whitelist nil :largeFileSize nil)
                                               :codeLens
                                               (:renderInline :json-false :localVariables t)
                                               :treeViews
                                               (:doubleClickTimeoutMs 500)
                                               :theme
                                               (:light
                                                (:skippedRange
                                                 (:textColor "rgb(100, 100, 100)" :backgroundColor "rgba(220, 220, 220, 0.3)"))
                                                :dark
                                                (:skippedRange
                                                 (:textColor "rgb(100, 100, 100)" :backgroundColor "rgba(18, 18, 18, 0.3)")))
                                               :workspaceSymbol
                                               (:caseSensitivity 1 :maxNum nil)
                                               :statusUpdateInterval 2000)
                                              :dart
                                              (:closingLabels t :analysisServerFolding t :analysisExcludedFolders
                                                              []
                                                              :debugSdkLibraries :json-false :debugExternalLibraries :json-false :enableCompletionCommitCharacters :json-false :triggerSignatureHelpAutomatically :json-false :flutterTrackWidgetCreation :json-false :flutterDocsHost "docs.flutter.io" :evaluateGettersInDebugViews t :flutterDebuggerRestartBehaviour "hotReload" :sdkPath "" :sdkPaths
                                                              []
                                                              :doNotFormat
                                                              []
                                                              :lineLength 80 :maxLogLineLength 2000 :insertArgumentPlaceholders t :showTestCodeLens t :showTodos t :openTestView
                                                              ["testRunStart"]
                                                              :reportAnalyzerErrors t :allowAnalytics t :checkForSdkUpdates t :pubAdditionalArgs
                                                              []
                                                              :runPubGetOnPubspecChanges t :promptToGetPackages t :flutterScreenshotPath "" :flutterSdkPath "" :flutterSdkPaths
                                                              []
                                                              :flutterHotReloadOnSave t :flutterCreateOrganization "" :flutterCreateIOSLanguage nil :flutterCreateAndroidLanguage nil :analyzeAngularTemplates t :normalizeWindowsDriveLetters t :analyzerAdditionalArgs
                                                              []
                                                              :analyzerDiagnosticsPort 0 :analyzerInstrumentationLogFile "" :analyzerSshHost nil :extensionLogFile "" :analyzerLogFile "" :analyzerObservatoryPort 0 :analyzerPath nil :flutterDaemonLogFile "" :flutterRunLogFile "" :flutterTestLogFile "" :pubTestLogFile "" :observatoryLogFile "" :vmAdditionalArgs
                                                              []
                                                              :buildRunnerAdditionalArgs
                                                              []
                                                              :flutterSelectDeviceWhenConnected t :warnWhenEditingFilesOutsideWorkspace t :showIgnoreQuickFixes :json-false :previewBuildRunnerTasks :json-false :previewToStringInDebugViews :json-false :promptToRunIfErrors t)
                                              :vetur
                                              (:completion
                                               (:autoImport t :useScaffoldSnippets t :tagCasing "kebab")
                                               :grammar
                                               (:customBlocks
                                                (:docs "md" :i18n "json"))
                                               :validation
                                               (:template t :style t :script t)
                                               :format
                                               (:options
                                                (:tabSize 2 :useTabs :json-false)
                                                :defaultFormatter
                                                (:html "prettyhtml" :css "prettier" :postcss "prettier" :scss "prettier" :less "prettier" :stylus "stylus-supremacy" :js "prettier-eslint" :ts "prettier")
                                                :defaultFormatterOptions
                                                (:js-beautify-html
                                                 (:wrap_attributes "force-expand-multiline")
                                                 :prettyhtml
                                                 (:printWidth 80 :singleQuote :json-false :wrapAttributes :json-false :sortAttributes :json-false))
                                                :styleInitialIndent :json-false :scriptInitialIndent :json-false)
                                               :trace
                                               (:server "verbose")
                                               :dev
                                               (:vlsPath ""))
                                              :concourse
                                              (:ls
                                               (:java
                                                (:heap "" :home "")))
                                              :cloudfoundry-manifest
                                              (:ls
                                               (:java
                                                (:heap "" :home "")))
                                              :boot-java
                                              (:boot-hints
                                               (:on t)
                                               :support-spring-xml-config
                                               (:on :json-false)
                                               :highlight-codelens
                                               (:on t)
                                               :change-detection
                                               (:on t)
                                               :remote-apps
                                               [])
                                              :spring-boot
                                              (:ls
                                               (:java
                                                (:home nil :heap nil)))
                                              :java
                                              (:home nil :jdt
                                                     (:ls
                                                      (:vmargs "-noverify -Xmx1G -XX:+UseG1GC -XX:+UseStringDeduplication"))
                                                     :errors
                                                     (:incompleteClasspath
                                                      (:severity "ignore"))
                                                     :configuration
                                                     (:checkProjectSettingsExclusions t :updateBuildConfiguration "interactive" :maven
                                                                                      (:userSettings nil))
                                                     :trace
                                                     (:server "verbose")
                                                     :import
                                                     (:gradle
                                                      (:enabled t)
                                                      :maven
                                                      (:enabled t)
                                                      :exclusions
                                                      ["**/node_modules/**" "**/.metadata/**" "**/archetype-resources/**" "**/META-INF/maven/**"])
                                                     :referencesCodeLens
                                                     (:enabled t)
                                                     :signatureHelp
                                                     (:enabled t)
                                                     :implementationsCodeLens
                                                     (:enabled t)
                                                     :format
                                                     (:enabled t :settings
                                                               (:url nil :profile nil)
                                                               :comments
                                                               (:enabled t)
                                                               :onType
                                                               (:enabled t))
                                                     :saveActions
                                                     (:organizeImports t)
                                                     :contentProvider
                                                     (:preferred nil)
                                                     :autobuild
                                                     (:enabled t)
                                                     :maxConcurrentBuilds 1 :completion
                                                     (:enabled t :overwrite t :guessMethodArguments :json-false :favoriteStaticMembers
                                                               ["org.junit.Assert.*" "org.junit.Assume.*" "org.junit.jupiter.api.Assertions.*" "org.junit.jupiter.api.Assumptions.*" "org.junit.jupiter.api.DynamicContainer.*" "org.junit.jupiter.api.DynamicTest.*" "org.mockito.Mockito.*" "org.mockito.ArgumentMatchers.*" "org.mockito.Answers.*"]
                                                               :importOrder
                                                               ["java" "javax" "com" "org"])
                                                     :progressReports
                                                     (:enabled t)
                                                     :debug
                                                     (:logLevel "verbose" :settings
                                                                (:showHex :json-false :showStaticVariables t :showQualifiedNames t :maxStringLength 0 :enableHotCodeReplace t :enableRunDebugCodeLens t :forceBuildBeforeLaunch t))
                                                     :dependency
                                                     (:showOutline t :syncWithFolderExplorer t :packagePresentation "flat")
                                                     :test
                                                     (:report
                                                      (:position "sideView")
                                                      :log
                                                      (:level "info")
                                                      :message
                                                      (:hintForDeprecatedConfig t :hintForSetingDefaultConfig t)
                                                      :defaultConfig "" :config nil))
                                              :rust-client
                                              (:logToFile :json-false :rustupPath "rustup" :rlsPath nil :revealOutputChannelOn "never" :updateOnStartup :json-false :disableRustup :json-false :channel nil)
                                              :rust
                                              (:sysroot nil :target nil :rustflags nil :clear_env_rust_log t :build_lib nil :build_bin nil :cfg_test :json-false :unstable_features :json-false :wait_to_build 1500 :show_warnings t :use_crate_blacklist t :build_on_save :json-false :features
                                                        []
                                                        :all_features :json-false :no_default_features :json-false :goto_def_racer_fallback :json-false :racer_completion t :clippy_preference "opt-in" :jobs nil :all_targets t :target_dir nil :rustfmt_path nil :build_command nil)
                                              :vsintellicode
                                              (:python
                                               (:completionsEnabled t)
                                               :java
                                               (:completionsEnabled :json-false)
                                               :typescript
                                               (:completionsEnabled :json-false))
                                              :maven
                                              (:excludedFolders
                                               ["**/.*" "**/node_modules" "**/target" "**/bin"]
                                               :executable
                                               (:preferMavenWrapper "true" :path "" :options "")
                                               :terminal
                                               (:useJavaHome :json-false :customEnv
                                                             [])
                                               :view "flat")
                                              :spring
                                              (:initializr
                                               (:serviceUrl "https://start.spring.io/" :defaultLanguage "" :defaultGroupId "com.example" :defaultArtifactId "demo" :defaultPackaging "JAR"))
                                              :\[git-commit\]
                                              (:editor\.rulers
                                               [72])
                                              :\[go\]
                                              (:editor\.insertSpaces :json-false)
                                              :\[json\]
                                              (:editor\.quickSuggestions
                                               (:strings t))
                                              :\[makefile\]
                                              (:editor\.insertSpaces :json-false)
                                              :\[markdown\]
                                              (:editor\.wordWrap "on" :editor\.quickSuggestions :json-false)
                                              :\[yaml\]
                                              (:editor\.insertSpaces t :editor\.tabSize 2 :editor\.autoIndent :json-false)
                                              :\[dart\]
                                              (:editor\.tabSize 2 :editor\.insertSpaces t :editor\.detectIndentation :json-false)
                                              :\[python\]
                                              (:editor\.suggestSelection "first")
                                              :\[javascript\]
                                              (:editor\.suggestSelection "first")
                                              :\[javascriptreact\]
                                              (:editor\.suggestSelection "first")
                                              :\[typescript\]
                                              (:editor\.suggestSelection "first")
                                              :\[typescriptreact\]
                                              (:editor\.suggestSelection "first")
                                              :\[java\]
                                              (:editor\.suggestSelection "first")
                                              :gitlens
                                              (:advanced
                                               (:messages
                                                (:suppressFileNotUnderSourceControlWarning t)))
                                              :mssql
                                              (:connections
                                               [(:server "localhost" :database "" :authenticationType "SqlLogin" :user "sa" :password "" :emptyPasswordInput :json-false :savePassword t :profileName "test")])
                                              :\[xml\] nil :xml
                                              (:trace
                                               (:server "verbose"))
                                              :julia
                                              (:executablePath "/home/kyoncho/.julia/julia-1.0.2/bin/julia")))

                  :server-id 'vls
                  :initialized-fn 'my-lsp-set-cfg))

(setq lsp-language-id-configuration '((java-mode . "java")
                                      (python-mode . "python")
                                      ;; (gfm-view-mode . "markdown")
                                      ;; (markdown-view-mode . "markdown")
                                      (markdown-mode . "markdown")
                                      (rust-mode . "rust")
                                      (css-mode . "css")
                                      (less-mode . "less")
                                      (less-css-mode . "less")
                                      (sass-mode . "sass")
                                      (scss-mode . "scss")
                                      (xml-mode . "xml")
                                      (c-mode . "c")
                                      (c++-mode . "cpp")
                                      (objc-mode . "objective-c")
                                      (web-mode . "vue")
                                      (html-mode . "html")
                                      (sgml-mode . "html")
                                      (mhtml-mode . "html")
                                      (go-mode . "go")
                                      (haskell-mode . "haskell")
                                      (php-mode . "php")
                                      (json-mode . "json")
                                      (rjsx-mode . "javascript")
                                      (js2-mode . "javascript")
                                      (typescript-mode . "typescript")
                                      (reason-mode . "reason")
                                      (caml-mode . "ocaml")
                                      (tuareg-mode . "ocaml")
                                      (swift-mode . "swift")
                                      (elixir-mode . "elixir")
                                      (conf-javaprop-mode . "spring-boot-properties")
                                      (yaml-mode . "spring-boot-properties-yaml")
                                      (ruby-mode . "ruby")
                                      (enh-ruby-mode . "ruby")
                                      (f90-mode . "fortran")))

(provide 'vls)
;;; prettierjs.el ends here
