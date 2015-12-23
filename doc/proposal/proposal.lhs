\documentclass{beamer}

%include lhs2TeX.fmt
%include polycode.fmt

%format ghci = "\lambda{>}\, "
%%format forall a = "\forall " a
%%format . = ".\ "

\author{Robert Hensing \and Robin Kuipers \and Jelle Postma}
\title{Orchestration Proposal Presentation}
\begin{document}


\begin{frame}
\maketitle
\end{frame}


\begin{frame}
\frametitle{Introduction}
\begin{itemize}
\item Foo
\item Bar
\item Baz
\end{itemize}
\end{frame}


\begin{frame}
\frametitle{Example}

> foo 1   = "foo"
> foo 20  = "bar"
> foo _   = "baz"

Qux:

> ghci foo 0
> "baz"

\end{frame}
\end{document}
