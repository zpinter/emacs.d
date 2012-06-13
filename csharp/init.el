(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
(append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(setq auto-mode-alist (cons '("\.xaml$" . nxml-mode) auto-mode-alist))