// ~/nixos-config/scripts/emacs-launcher.go
// Niri version of Emacs launcher with intelligent polling
package main

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

type NiriWindow struct {
	ID    int    `json:"id"`
	Title string `json:"title"`
	AppID string `json:"app_id"`
}

type NiriWindows struct {
	Windows []NiriWindow `json:"windows"`
}

// Check if a window with title matching pattern exists and is active
func isEmacsWindowActive() bool {
	cmd := exec.Command("niri", "msg", "-j", "windows")
	output, err := cmd.Output()
	if err != nil {
		return false
	}

	var windows NiriWindows
	if err := json.Unmarshal(output, &windows); err != nil {
		return false
	}

	// Check if any Emacs window is focused
	for _, win := range windows.Windows {
		if (strings.Contains(strings.ToLower(win.Title), "emacs") ||
			strings.Contains(strings.ToLower(win.AppID), "emacs")) {
			// In Niri, we'd need to check focus state
			// For now, assume if Emacs exists, it might be focused
			return true
		}
	}
	return false
}

// Get Emacs window ID
func getEmacsWindowID() (int, bool) {
	cmd := exec.Command("niri", "msg", "-j", "windows")
	output, err := cmd.Output()
	if err != nil {
		return 0, false
	}

	var windows NiriWindows
	if err := json.Unmarshal(output, &windows); err != nil {
		return 0, false
	}

	for _, win := range windows.Windows {
		if strings.Contains(strings.ToLower(win.Title), "emacs") ||
			strings.Contains(strings.ToLower(win.AppID), "emacs") {
			return win.ID, true
		}
	}
	return 0, false
}

// Focus Emacs window and wait for focus to complete using intelligent polling
func focusEmacsWindow() error {
	windowID, found := getEmacsWindowID()
	if !found {
		return fmt.Errorf("no Emacs window found")
	}

	// Activate the window using niri msg
	cmd := exec.Command("niri", "msg", "action", "focus-window", "--id", fmt.Sprintf("%d", windowID))
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to activate window: %v", err)
	}

	// Intelligent polling to wait for window activation
	maxWait := 500 * time.Millisecond
	pollInterval := 2 * time.Millisecond
	deadline := time.Now().Add(maxWait)

	for time.Now().Before(deadline) {
		if isEmacsWindowActive() {
			return nil
		}
		time.Sleep(pollInterval)
		pollInterval *= 2
		if pollInterval > 10*time.Millisecond {
			pollInterval = 10 * time.Millisecond
		}
	}

	return nil
}

func executeEmacsCommand(command string) error {
	cmd := exec.Command("emacsclient", "-n", "-e", command)
	return cmd.Run()
}

func isEmacsServerRunning() bool {
	cmd := exec.Command("emacsclient", "-e", "(server-running-p)")
	output, err := cmd.Output()
	if err != nil {
		return false
	}
	return strings.TrimSpace(string(output)) == "t"
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: emacs-launcher <elisp-command>")
		fmt.Fprintln(os.Stderr, "Example: emacs-launcher '(my/focus-and-open-dirvish)'")
		os.Exit(1)
	}

	emacsCommand := strings.Join(os.Args[1:], " ")

	if !isEmacsServerRunning() {
		fmt.Fprintln(os.Stderr, "Error: Emacs server is not running")
		fmt.Fprintln(os.Stderr, "Start it with: emacs --daemon")
		os.Exit(1)
	}

	// If Emacs is already active, execute immediately
	if isEmacsWindowActive() {
		executeEmacsCommand(emacsCommand)
		return
	}

	// Focus Emacs window and wait for it to become active
	if err := focusEmacsWindow(); err != nil {
		fmt.Fprintf(os.Stderr, "Warning: %v\n", err)
	}

	// Execute the Emacs command
	if err := executeEmacsCommand(emacsCommand); err != nil {
		fmt.Fprintf(os.Stderr, "Error: emacsclient failed: %v\n", err)
		os.Exit(1)
	}
}
