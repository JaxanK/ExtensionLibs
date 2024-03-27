(ns jaxank.ExtensionLibs.SampleCode.SystemTray
  (:require [jaxank.ExtensionLibs.JaxExtension :as je]
            [clj-systemtray.core :as tray]   ;Add this to deps.edn    clj-systemtray/clj-systemtray {:mvn/version "0.2.1"}
            )
  (:import ;[java.awt SystemTray TrayIcon Toolkit PopupMenu MenuItem Menu]
   [java.awt.event ActionListener]))


(def Icon (atom nil))

(def IconPath "Logo.png") ; Must be a .PNG, JPEG, or there is one other file type, but not an .ico file!
(def IconTooltip "Program Name")

(defn MainClickFn "the main function to call on a left click of the icon" [_event]
  (println "Main Click!"))

(def PopupMenu (tray/popup-menu

                (tray/menu-item "Menu Item 1" MainClickFn)
                (tray/separator)
                (tray/menu-item (str "Version: " "1.01") (fn [_])) ; Just using this for text even though it is a button - might be another way to do this
                (tray/separator)
                (tray/menu "Commands"
                           (tray/menu-item "SubMenu Item 1" (fn [& _] (println "Sub Menu Item 1 Called!")))
                           (tray/menu-item "SubMenu Item 2" (fn [& _] (println "Sub Menu Item 2 Called!")))
                           (tray/separator)
                           (tray/menu-item "Kill Controller"  (fn [& _] (println "Sub Menu Item 3 Called!"))))
                (tray/menu-item "Exit Thrawn" (fn [& _] (println "Exit the program...")))))


(defn StartSystemTray! "Call this to start the SystemTray Icon and Submenu (will automatically replace existing TrayIcon)" []
  ;Stop previous Icon if it is running
  (if @Icon (tray/remove-tray-icon! @Icon))

  (reset! Icon (tray/make-tray-icon! IconPath PopupMenu))

  ;Set the Tool tip (shows up on mouse hover)
  (.setToolTip @Icon IconTooltip)

  ;If Icon is left clicked, MainClickFn will get called
  (doto @Icon
    (.addActionListener
     (proxy [ActionListener] []
       (actionPerformed [event]
         (MainClickFn event))))))

(defn StopSystemTray! "Call this to stop the SystemTray Icon and Submenu" []
  (if @Icon (tray/remove-tray-icon! @Icon)))


(defn DisplaySystemTrayMessage "Display system tray notification. Not supported on all platforms (by Java classes). \ntype can be #{:none :info :warning :error}" [heading message type]
  (let [heading (if (string? heading) heading  "")
        message (if (string? message) message  "")
        type    (if (keyword?   type)    type  :none)]

    (tray/display-message @Icon heading message type)))

(comment
  (StartSystemTray!))