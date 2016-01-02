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
  \frametitle{Orchestration?}

  \only<2->{
    Wikipedia: Orchestration describes the automated arrangement,
    coordination, and management of complex computer systems, middleware
    and services.
  }
% -- Wikipedia "Orchestration_(computing), 2016-01-01
% Wikipedia rambles on with many it operations buzzwords, but there may be something actually useful in there for this presentation...

\end{frame}

\begin{frame}
  \frametitle{Taxonomy}
  Wikipedia level research :)

% TODO: academic research?
% Wikipedia has some references that may be interesting
% https://en.wikipedia.org/wiki/Orchestration_%28computing%29#References
\end{frame}


\begin{frame}
  \frametitle{Taxonomy}

  \begin{itemize}
    \item Arrangement
       \only<2>{
         \begin{itemize}
           \item Allocating machines
           \item Deploying applications
         \end{itemize}
       }
    \item Coordination
      \only<3>{
        \begin{itemize}
% FIXME: is this correct?
          \item Peer discovery in distributed app?
        \end{itemize}
      }
    \item Management
      \only<4>{
        \begin{itemize}
% FIXME: does this make sense?
          \item Monitoring
          \item Configuration
        \end{itemize}
      }
  \end{itemize}
\end{frame}


\begin{frame}
  \frametitle{Existing products ...}

  ... from the hadoop community

  \begin{description}
    \item[ZooKeeper] A coordination service for distributed applications
    \item[Ambari] Web UI for provisioning, managing and monitoring other hadoop products
    \item[Chukwa] A monitoring / data collection system piggybacking on the storage and compute systems in hadoop
  \end{description}

\end{frame}

\begin{frame}
  \frametitle{ZooKeeper}

% The ZooKeeper core is a very nice application of concurrent programming

% TODO: slide

\end{frame}

\begin{frame}
  \frametitle{Ambari}

% Integrates with many components. Feasible subset for project?

% TODO: slide

\end{frame}

\begin{frame}
  \frametitle{Chukwa}

% Not a particularly good candidate for a project. Builds on storage
% and computation systems. Would be an exercise in programming the logic
% layer without having an actual back-end (which is interesting, but not ideal)

% TODO: slide

\end{frame}


\begin{frame}
\frametitle{Choice of project}

TimeKeeper

A distributed data store for cloud management

Focus on consistency, availability, scalability.

Not unlike ZooKeeper

\end{frame}


\begin{frame}
\frametitle{$\qed$.}
Questions?

\end{frame}
\end{document}
