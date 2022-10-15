# Navigate through large numbers of large documents

The `study.el` system is a collection of packages that allow you to navigate through a large number of documents that may contain a lot of content.

Currently, only PDFs are supported, but `study.el` can be extended to support many different content types using the same generic Emacs interface.

## Limitations

The main document viewer currently supported by `study.el`, Okular, uses dbus for inter-process-communication. This means you will have to have dbus running on your system to make use of `study.el`

## The included packages

+ `study.el` :: generic functions for working with documents
+ `study-dired.el` :: open documents from dired
+ `study-deadgrep.el` :: search through and open documents using deadgrep and ripgrep-all
+ `study-SyncTeX.el` :: use Okular as a SyncTeX viewer through `study.el`
+ `study-okular.el` :: use Okular as a `study.el` client
+ `study-generic.el` :: generic interface for `study.el` clients, look here if you want to implement your own
