// ~/nixos-config/scripts/emacs-launcher.go
// Enhanced Niri version with popup and focus modes
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

	for _, win := range windows.Windows {
		if (strings.Contains(strings.ToLower(win.Title), "emacs") ||
			strings.Contains(strings.ToLower(win.AppID), "emacs")) {
			return true
		}
	}
	return false
}

// Get main Emacs window ID (not popup windows)
func getMainEmacsWindowID() (int, bool) {
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
		// Skip popup/float windows, look for main Emacs instance
		if (strings.Contains(strings.ToLower(win.Title), "emacs") ||
			strings.Contains(strings.ToLower(win.AppID), "emacs")) &&
			!strings.Contains(strings.ToLower(win.Title), "popup") &&
			!strings.Contains(strings.ToLower(win.Title), "float") {
			return win.ID, true
		}
	}
	return 0, false
}

// Focus main Emacs window using intelligent polling
func focusMainEmacsWindow() error {
	windowID, found := getMainEmacsWindowID()
	if !found {
		return fmt.Errorf("no main Emacs window found")
	}

	cmd := exec.Command("niri", "msg", "action", "focus-window", "--id", fmt.Sprintf("%d", windowID))
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to activate window: %v", err)
	}

	// Intelligent polling
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
		fmt.Fprintln(os.Stderr, "Usage: emacs-launcher [--focus] <elisp-command>")
		fmt.Fprintln(os.Stderr, "")
		fmt.Fprintln(os.Stderr, "Options:")
		fmt.Fprintln(os.Stderr, "  --focus    Focus main Emacs window before executing command")
		fmt.Fprintln(os.Stderr, "             (use for capture, omit for popup windows)")
		fmt.Fprintln(os.Stderr, "")
		fmt.Fprintln(os.Stderr, "Examples:")
		fmt.Fprintln(os.Stderr, "  # Popup window (dirvish, notes):")
		fmt.Fprintln(os.Stderr, "  emacs-launcher '(my/dirvish-popup)'")
		fmt.Fprintln(os.Stderr, "")
		fmt.Fprintln(os.Stderr, "  # Focus main window (capture):")
		fmt.Fprintln(os.Stderr, "  emacs-launcher --focus '(my/focus-and-capture)'")
		os.Exit(1)
	}

	if !isEmacsServerRunning() {
		fmt.Fprintln(os.Stderr, "Error: Emacs server is not running")
		fmt.Fprintln(os.Stderr, "Start it with: emacs --daemon")
		os.Exit(1)
	}

	// Parse arguments
	shouldFocus := false
	commandStart := 1

	if os.Args[1] == "--focus" {
		shouldFocus = true
		commandStart = 2
		if len(os.Args) < 3 {
			fmt.Fprintln(os.Stderr, "Error: No command provided after --focus")
			os.Exit(1)
		}
	}

	emacsCommand := strings.Join(os.Args[commandStart:], " ")

	// If focus flag is set, focus main Emacs window first
	if shouldFocus {
		if err := focusMainEmacsWindow(); err != nil {
			fmt.Fprintf(os.Stderr, "Warning: %v\n", err)
		}
		// Small delay to ensure focus is complete
		time.Sleep(50 * time.Millisecond)
	}

	// Execute the Emacs command
	if err := executeEmacsCommand(emacsCommand); err != nil {
		fmt.Fprintf(os.Stderr, "Error: emacsclient failed: %v\n", err)
		os.Exit(1)
	}
}
