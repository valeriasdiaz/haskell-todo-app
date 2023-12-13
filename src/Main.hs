{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow
import Data.Time.Calendar.OrdinalDate
import Monomer.Core.ThemeTypes
import Monomer.Core.Themes.BaseTheme
import Monomer.Graphics

import qualified Monomer.Lens as L


--i'm guessing i need to make something like this i guess
data TodoModel = TodoModel {
  _tasky :: Text,
  _taskll :: Text,
  _datell :: Text,
  _tasktf :: Bool,
  _datee :: Text
} deriving (Eq, Show)


data TodoEvent = TodoInit 
      | TodoCompleted 
      | TodoSubmit
      | TodoDelete
  deriving (Eq, Show)

makeLenses 'TodoModel

buildUI
  :: WidgetEnv TodoModel TodoEvent
  -> TodoModel
  -> WidgetNode TodoModel TodoEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Welcome to your personal task manager!" `styleBasic` [textFont "Bold", textSize 35, textColor (rgbHex "#FF0A54")],
      spacer,
      label "Task and due date: " `styleBasic` [textFont "Bold", textSize 25, textColor (rgbHex "#FF477E")],
      spacer,
      hstack [
        textField_ tasky [placeholder "Type here :3"]
        `nodeKey` "description",
        textField_ datee [placeholder "Enter date here :3"]
      ],
      -- dateField datee,
      spacer,
      button "Add to-do" TodoSubmit `styleBasic` [textFont "Bold"],
      spacer,
      hstack [
        spacer,
        checkbox tasktf,
        spacer,
        label ("" <> model ^. taskll) `styleBasic` [styleIf (model ^. tasktf) (textColor (rgbHex "#47257E")), styleIf (not (model ^. tasktf)) (textColor (rgbHex "#FF477E")), textSize 20, textFont "Bold"],
        filler,
        label ("" <> model ^. datell) `styleBasic` [styleIf (model ^. tasktf) (textColor (rgbHex "#47257E")), styleIf (not (model ^. tasktf)) (textColor (rgbHex "#FF477E")), textSize 20, textFont "Bold"],
        spacer,
        button "Delete" TodoDelete
        --label $ "Click count: " <> showt (model ^. clickCount),
      ]
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv TodoModel TodoEvent
  -> WidgetNode TodoModel TodoEvent
  -> TodoModel
  -> TodoEvent
  -> [AppEventResponse TodoModel TodoEvent]
handleEvent wenv node model evt = case evt of
  -- This is used for the app's initialization, yeah
  TodoInit -> []
  -- This method is an event handler that is called when I
  -- click the 'Add to-do' button for my task. What this does 
  -- is that the variable taskll and datell are set to the values
  -- that are inputted to the text boxes, and these are the
  -- the variables I use to display the task.
  TodoSubmit -> [
    Model $ model
      & taskll .~ (model ^. tasky)
      & datell .~ (model ^. datee)
    ]
  -- This method is an event handler that is called
  -- when I click the 'delete' button for my task.
  -- What this does is that it sets the 2 variables
  -- I use to display the entered task to empty strings,
  -- to simulate them as having been "deleted".
  TodoDelete -> [
    Model $ model
      & taskll .~ ""
      & datell .~ ""
    ]
  -- AppIncrease -> [Model (model & clickCount +~ 1)]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    -- This sets any configurations/settings of the appearance
    -- of your GUI. Here, I am setting the title of the GUI
    -- window, the app icon, the theme, and any fonts I need.
    config = [
      appWindowTitle "To-do App",
      appWindowIcon "./assets/images/notepad-icon.png",
      appTheme customTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appFontDef "Italic" "./assets/fonts/Roboto-Italic.ttf",
      appInitEvent TodoInit
      ]
    -- This line initializes the variables of the model
    -- I am using to default values.
    model = TodoModel {
      _tasky = "",
      _taskll = "",
      _datell = "",
      _tasktf = False,
      _datee = ""
    }

-- The customTheme function is one that creates a new theme, with each variable representing
-- a component of the GUI. With this function, I can set these components to be the color I
-- them to be.
customTheme :: Theme
customTheme = baseTheme darkThemeColors {
  clearColor = rgbHex "#EDE4F1",
  btnMainBgBasic = rgbHex "#EE9000",
  btnBgHover = rgbHex "#FF477E",
  btnMainBgFocus = rgbHex "#FFA500",
  btnMainBgActive = rgbHex "#ffdee8",
  btnMainBgDisabled = rgbHex "#BB8800",
  btnMainText = rgbHex "000000",
  inputBgBasic = rgbHex "#FAE0E4",
  inputFgBasic = rgbHex "#800F2F",
  inputText = rgbHex "#800F2F",
  btnBgActive = rgbHex "#FF99AC",
  slMainBg = rgbHex "#000000",
  dialogText = black,
  sectionColor = rgbHex "FF99AC",
  btnBgBasic = rgbHex "#FF99AC",
  btnBgFocus = rgbHex "#FF5C8A"
}