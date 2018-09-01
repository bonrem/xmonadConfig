--
--Bonrem Made It--
--May break your system do it at your own peril--
--Ps configured for dualhead may not work on other configs ,
--However, you are welcome to try and make any necessary adjustmets.
-- if you are interested in  looking at the library source code see http://mcs.une.edu.au/doc/ghc/html/libraries/xmonad-contrib-0.13/
--   
import XMonad
import XMonad.Core 
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicProperty			-- manage window profile and actions
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)  --Adding key bindings
import XMonad.Util.Loggers  				 -- log battery N' stuff
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes 
import XMonad.Util.Timer
import XMonad.Util.WorkspaceCompare
import System.IO
import System.Exit
import qualified XMonad.StackSet as S
import qualified Data.Map as M
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.OnScreen
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer          -- dmenu ops to bring apps to your current window     
import XMonad.Actions.TopicSpace			 -- turn workspace into topic oriented
import XMonad.Actions.Submap				 -- add own key bindings                   
import XMonad.Layout.Spacing				 -- spacing between windows 
import XMonad.Layout.LimitWindows			 -- window no limiter 	
import XMonad.Layout.Gaps                    -- Add gaps on screen
import XMonad.Layout.Mosaic					 -- mosaic Layout
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import Graphics.X11.ExtraTypes.XF86          --XF86 keys
import Data.Monoid
import Data.Maybe ( isJust, catMaybes, mapMaybe )
import Data.List (intersperse, stripPrefix, isPrefixOf, sortBy)
import Codec.Binary.UTF8.String (encodeString)
import qualified XMonad.Layout.IndependentScreens as LIS
--  | MainExecution |
main =  do
			
		dzenBar1 <- spawnPipe "~/.xmonad/Bash/bBar"
		dzenBar2 <- spawnPipe "~/.xmonad/Bash/bBar2"
		xmonad $ defaultConfig {
 				--basic
				borderWidth			= bBorderWidth
				,terminal			= bTerminal
				,normalBorderColor	= bNormalBorderColor
				,focusedBorderColor	= bFocusedBorderColor
				,modMask			= myModMask
				,startupHook		= startup 
				,handleEventHook    = clockEventHook
				,workspaces			= bWorkspaces
				,manageHook			= 
									  manageHook defaultConfig <+> 
									  bmanageHook
				,logHook           	= 
									  bLogHookLeft dzenBar1 <+>
									  bLogHookRight dzenBar2				
				,keys				= bKeys
				,layoutHook         = blayoutHook 
									 
						}


--  | PreferedPrograms |
bTerminal	= "st"
bBrowser 	= "vivaldi-stable"


--   | keybindings |
myModMask	= mod4Mask
altMask 	= mod1Mask
--fnMask      = mod3Mask 				--mod3 is menu key and has to be added using xmodmap
				-- config
bKeys conf = M.fromList $
    [
    --keys to Apps 
      
      ((myModMask				, xK_Return	),spawn bTerminal )
	, ((myModMask				, xK_p		),spawn "~/.xmonad/Bash/menu")		--Start dmenu from '~/.xmonad/menu.sh' (why:ease in dmenu config)
    , ((myModMask               , xF86XK_AudioRaiseVolume ),	spawn "amixer set Master 5%+")
    , ((myModMask               , xF86XK_AudioLowerVolume ),	spawn "amixer set Master 5%-")
    , ((myModMask               , xF86XK_AudioMute        ),	spawn "amixer set Master toggle")
	, ((0, xF86XK_Mail),  submap . M.fromList $ [ ((0, xF86XK_Mail),  spawn bBrowser)])
	--, ((0, Pause      ),  submap . M.fromList $ [ ((0, Pause      ),  spawn bBrowser)])
    --Navigation Made easier

    , ((altMask					, xK_F4		), kill)
    , ((myModMask				, xK_space	), sendMessage NextLayout)
    , ((myModMask				, xK_n		), refresh)
    			--window navigation
    , ((altMask				    , xK_Up		), windows S.swapMaster)
    , ((altMask 				, xK_Left	), windows S.swapUp)
    , ((altMask	     			, xK_Right	), windows S.swapDown)
    , ((altMask					, xK_Tab	), windows S.focusDown) 
    , ((altMask .|. shiftMask	, xK_Tab	), windows S.focusUp)
    , ((myModMask 				, xK_Left	), sendMessage Shrink)
    , ((myModMask				, xK_Right	), sendMessage Expand)
    , ((myModMask				, xK_q		), broadcastMessage ReleaseResources >> restart "xmonad" True)
    			--workspace manupulation  bindings 
    , ((altMask					, xK_p     	), gotoMenu ) 	--go to app
	, ((altMask					, xK_b     	), bringMenu) 	--bring	app	to	workspace
	, ((controlMask             , xK_equal  ), sendMessage Mag.MagnifyMore)
	, ((controlMask             , xK_minus  ), sendMessage Mag.MagnifyLess)
	, ((controlMask .|. shiftMask           , xK_equal  ),sendMessage Mag.Toggle)
	                   
				-- Quit xmonad
	 , ((myModMask .|. altMask 	, xK_q      ), io (exitWith ExitSuccess))
    --Lock Screen
    , ((myModMask              , xF86XK_HomePage       ), spawn "dm-tool switch-to-greeter") --requires  lightdm
    -- Push window back into tiling
    , ((myModMask              , xK_s       ), withFocused $ windows . S.sink) 
    ]

	++ --move window to specific workspace
    [((myModMask .|. shiftMask   , k        ), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(S.greedyView, 0), (S.shift, shiftMask)]]
	--Workspaces multi-head config : keep workspace one on screen 1
	++
	 [ ((m .|. myModMask, k), windows (f i))
	        | (i, k) <- zip (workspaces conf) ([xK_1 .. xK_9] )
	        , (f, m) <- [ (viewOnScreen 1, 0)
	                      ,(viewOnScreen 0, controlMask)
	                     ,(S.greedyView, controlMask .|. shiftMask)]
	                     ]
                         
--	|	MouseBindings	|

focusFollowsMouse :: Bool 		--	()MousePointerFocus	
focusFollowsMouse	= False		--mouse follow focus

clickJustFocuses :: Bool
clickJustFocuses	= True		--click focus
	

--	| WorkSpaces	|
bWorkspaces		= ["I","II","III","Art","www","Media","Fun","Projects"]

bTopics :: [Topic]
bTopics=["I","II","III","Art","www","Media","Projects","Business"]
--bTopicConfig = TopicConfig
--{ topicDirs = M.fromList $ [("I", "~/"),("II", "~/"),("III", "~/"),("Art", "~/Art"),("www", "~/"),("Media","~/Media"),("fun","~/"),("Projects","~/Projects")]
-- ,defaultTopicAction =const $ spawnShell>*> 2
-- ,defaultTopic = "I"
-- ,maxTopicHistory=10
-- ,topicActions = M.fromList $[("Art", spawnOn "Art" "blender" ),("www",spawnOn "www" "vivaldi-stable")]
--}
--spawnShell :: X ()
--spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

--spawnShellIn :: Dir -> X ()
--spawnShellIn dir = spawn $ "urxvt '(cd ''" ++ dir ++ "'' && " ++ myShell ++ " )'"

--goto :: Topic -> X ()
--goto = switchTopic myTopicConfig

--promptedGoto :: X ()
--promptedGoto = workspacePrompt myXPConfig goto

--promptedShift :: X ()
--promptedShift = workspacePrompt myXPConfig $ windows . W.shift

--	|WindowsRules::|	
--	() Window managment: 

bmanageHook	= composeAll . concat $ 
			 [
			 [isFullscreen						--> doFullFloat			         	   		]	 
			,[className 		=? c			--> doShift 		"www" 	| c <-bWeb		]
			,[className 		=? c			--> doShift 		"Media" | c <- bMedia 	]		
			,[className 	 	=? c			--> doShift 		"Art" 	| c <- bArt   	]
			,[className 		=? c			--> doFloat	 			  	| c <- bFloat 	]
			,[className			=? c			--> doFullFloat		 	  	| c <- bFullFloat ]
			]
			where 
				bFloat		= ["vlc","konsole","yakuake","plank","Guake"]
				bFullFloat	= []
				bWeb		= ["Chromium","Vivaldi-stable"]
				bMedia		= ["vlc"]
				bArt 		= ["Gimp","Blender","Lmms","tupitube","Freecad","QCAD","Viking","viking"]
		 

								--	| LookAndFeel:	|

-- ()   Layout::
bBorderWidth	= 1

--colors::
bNormalBorderColor 	= "#975d68" 
bFocusedBorderColor	= "#dddddd"

blayoutHook  =  gaps [(U,20), (D,18)] $ 
				smartSpacing 4 $
				limitWindows 10 $
				avoidStruts $ 
				--onWorkspace "www" Tall $ 
				Tall 1 (3/100) (1/2)||| mosaic 2 [3,2]   ||| Full  --gaps


--   | menu and status bars  |
	--apps Run with logCmd
bLogHookLeft :: Handle -> X ()
bLogHookLeft h = bLeftLogWithPP $  dzenPP 
    {
		 ppOutput           =   hPutStrLn h
      , ppVisible           =   dzenColor "green" "#1B1D1E"   	. pad
      , ppCurrent           =    wrap "[ "" ]" . dzenColor "yellow" "#1B1D1E"  
      , ppHidden            =   dzenColor "white" "#1B1D1E"		. pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E"  	. pad
      , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E"  	. pad
      , ppSep               =   "    "
      , ppLayout            =   dzenColor "#ebac54" "#1B1D1E"   .
                                (\x -> case x of
                                    "Spacing Tall"             ->      "Tall"
                                    "Spacing Mosaic"  		   ->      "Mosaic"
                                    "Spacing Full"             ->      "Full"
                                    "Simple Float"             ->      "~"
                                    _                          ->      x
                                )
      , ppTitle             =   ("  " ++) . dzenColor "blue" "#1B1D1E"  . wrap "| " "|" . trim  . dzenEscape . shorten 25
      , ppExtras 			= [logCmd "whoami" ]

      
    }
--bLogHookRight :: Handle -> X ()
bLogHookRight  dzenBar2	= bRightLogWithPP $ bPP
	{ ppOutput      =   hPutStrLn dzenBar2
	, ppSep			= 	" | "
	, ppExtras 		=[   logCmd "~/.xmonad/Bash/util --cpu "
						,logCmd "~/.xmonad/Bash/util --mem "
						,logCmd "~/.xmonad/Bash/util --disk "
						--,logCmd "~/.xmonad/Bash/util --disk "
						,logCmd "~/.xmonad/Bash/util --volume "
					 	,logCmd "~/.xmonad/Bash/util --time "	
						,logCmd "~/.xmonad/Bash/util --date"
						,logCmd "~/.xmonad/Bash/util --uptime "
						
						]
	}
bPP :: PP
bPP = def {        
                    ppSep      = ""
                   }
-- | Def ->  LOg  |
--
-- Define LOG   replaces dynamicLOg useful if you want to format the appearance of your status bar 
-- i.e you could include and exclude utilities in this case i have removed the title field 
-- insights from {http://mcs.une.edu.au/doc/ghc/html/libraries/xmonad-contrib-0.13}   codebase 
		-- |Right|
		-- only includes ppExtras
bRightLog :: X ()
bRightLog = bRightLogWithPP def

-- | Format the current status using the supplied pretty-printing format,
--   and write it to stdout.

bRightLogWithPP :: PP -> X ()
bRightLogWithPP pp = bRightLogString pp >>= io . ppOutput pp

bRightLogString :: PP -> X String
bRightLogString pp = do

    extras <- mapM (flip catchX (return Nothing)) $ ppExtras pp

    return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
                        [ 
                         --ppLayout pp ld
                        ]
                        ++ catMaybes extras 
-- |left |

bLeftLog :: X ()
bLeftLog = bLeftLogWithPP def

-- | Format the current status using the supplied pretty-printing format,
--   and write it to stdout.
bLeftLogWithPP :: PP -> X ()
bLeftLogWithPP pp = bLeftLogString pp >>= io . ppOutput pp

-- | The same as 'dynamicLogWithPP', except it simply returns the status
--   as a formatted string without actually printing it to stdout, to
--   allow for further processing, or use in some application other than
--   a status bar.
bLeftLogString :: PP -> X String
bLeftLogString pp = do

    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort pp

    -- layout description
    let ld = description . S.layout . S.workspace . S.current $ winset

    -- workspace list
    let ws = bpprWindowSet sort' urgents pp winset

    -- window title
    --wt <- maybe (return "") (fmap show . getName) . S.peek $ winset

    -- run extra loggers, ignoring any that generate errors.
    extras <- mapM (flip catchX (return Nothing)) $ ppExtras pp

    return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
                        [ ws
                        , ppLayout pp ld 
                        ]
                        ++ catMaybes extras

-- | Format the workspace information, given a workspace sorting function,
--   a list of urgent windows, a pretty-printer format, and the current
--   WindowSet.
bpprWindowSet :: WorkspaceSort -> [Window] -> PP -> WindowSet -> String
bpprWindowSet sort' urgents pp s = sepBy (ppWsSep pp) . map fmt . sort' $
            map S.workspace (S.current s : S.visible s) ++ S.hidden s
   where this     = S.currentTag s
         visibles = map (S.tag . S.workspace) (S.visible s)

         fmt w = printer pp (S.tag w)
          where printer | any (\x -> maybe False (== S.tag w) (S.findTag x s)) urgents  = ppUrgent
                        | S.tag w == this                                               = ppCurrent
                        | S.tag w `elem` visibles                                       = ppVisible
                        | isJust (S.stack w)                                            = ppHidden
                        | otherwise                                                     = ppHiddenNoWindows

sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = concat . intersperse sep . filter (not . null)       

-- HANDLE EVENT HOOK CONFIG                                                               --
-- Wrapper for the Timer id, so it can be stored as custom mutable state
data TidState = TID TimerId deriving Typeable 
instance ExtensionClass TidState where
	initialValue = TID 0
--startupHook
clockStartupHook = startTimer 1 >>= XS.put . TID
-- Handle event hook
clockEventHook e = do               -- e is the event we've hooked
  (TID t) <- XS.get                 -- get the recent Timer id
  handleTimer t e $ do              -- run the following if e matches the id
    startTimer 1 >>= XS.put . TID   -- restart the timer, store the new id
    ask >>= logHook.config          -- get the loghook and run it
    return Nothing                  -- return required type
  return $ All True  
-- |startup |

startup=
	(spawn "~/.xmonad/Bash/background")<+>
	( startTimer 1 >>= XS.put . TID )
-- Laptop  screens config
--togglevga = do
--  screencount <- LIS.countScreens
--  if screencount > 1
--   then spawn "xrandr --output VGA1 --off"
--   else spawn "xrandr --output VGA1 --auto --right-of LVDS1"
