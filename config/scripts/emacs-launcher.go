// ~/nixos-config/scripts/emacs-launcher.go
// KDE Plasma version of Emacs launcher with intelligent polling
package main

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

// Check if a window with title matching pattern exists and is active
func isEmacsWindowActive() bool {
	cmd := exec.Command("xdotool", "getactivewindow", "getwindowname")
	output, err := cmd.Output()
	if err != nil {
		return false
	}
	windowName := strings.TrimSpace(string(output))
	return strings.Contains(windowName, "Emacs") || strings.Contains(windowName, "emacs")
}

// Get Emacs window ID
func getEmacsWindowID() (string, bool) {
	cmd := exec.Command("xdotool", "search", "--class", "Emacs")
	output, err := cmd.Output()
	if err != nil {
		return "", false
	}

	windowIDs := strings.Split(strings.TrimSpace(string(output)), "\n")
	if len(windowIDs) > 0 && windowIDs[0] != "" {
		return windowIDs[0], true
	}
	return "", false
}

// Focus Emacs window and wait for focus to complete using intelligent polling
func focusEmacsWindow() error {
	windowID, found := getEmacsWindowID()
	if !found {
		return fmt.Errorf("no Emacs window found")
	}

	// Activate the window
	cmd := exec.Command("xdotool", "windowactivate", windowID)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to activate window: %v", err)
	}

	// Intelligent polling to wait for window activation
	// Start with fast polls, back off if taking longer
	maxWait := 500 * time.Millisecond
	pollInterval := 2 * time.Millisecond // Start very fast
	deadline := time.Now().Add(maxWait)

	for time.Now().Before(deadline) {
		if isEmacsWindowActive() {
			// Window is now active, we're done
			return nil
		}

		time.Sleep(pollInterval)

		// Exponential backoff: 2ms -> 4ms -> 8ms -> 10ms (cap)
		pollInterval *= 2
		if pollInterval > 10*time.Millisecond {
			pollInterval = 10 * time.Millisecond
		}
	}

	// Timeout reached - window probably activated anyway
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

	// Join all arguments in case command has spaces
	emacsCommand := strings.Join(os.Args[1:], " ")

	// Check if Emacs server is running
	if !isEmacsServerRunning() {
		fmt.Fprintln(os.Stderr, "Error: Emacs server is not running")
		fmt.Fprintln(os.Stderr, "Start it with: emacs --daemon")
		os.Exit(1)
	}

	// If Emacs is already active, execute immediately (no need to focus)
	if isEmacsWindowActive() {
		executeEmacsCommand(emacsCommand)
		return
	}

	// Focus Emacs window and wait for it to become active
	if err := focusEmacsWindow(); err != nil {
		// Window focus failed, but try to execute command anyway
		fmt.Fprintf(os.Stderr, "Warning: %v\n", err)
	}

	// Execute the Emacs command
	if err := executeEmacsCommand(emacsCommand); err != nil {
		fmt.Fprintf(os.Stderr, "Error: emacsclient failed: %v\n", err)
		os.Exit(1)
	}
}
