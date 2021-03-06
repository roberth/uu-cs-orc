\documentclass{beamer}

%include lhs2TeX.fmt
%include polycode.fmt

%format ghci = "\lambda{>}\, "
%%format forall a = "\forall " a
%%format . = ".\ "

\usepackage{graphicx}

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

% Wikipedia has some references that may be interesting
% https://en.wikipedia.org/wiki/Orchestration_%28computing%29#References


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
          \item Peer discovery
          \item Coordination
        \end{itemize}
      }
    \item Management
      \only<4>{
        \begin{itemize}
          \item Start/stop
          \item Monitoring
          \item Configuration
        \end{itemize}
      }
  \end{itemize}
\end{frame}


\begin{frame}
  \frametitle{Taxonomy}

  \includegraphics[width=10cm]{orchestration-lifecycle-flowchart.png} \\
  \hfill -- Open Data Center Alliance
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

  \begin{itemize}
    \item Very simple interface
    \item Maintenance, coordination, management, consensus\dots
    \item Synchronisation for distributed systems
    \item Aims to implement functionality that is hard to implement but easy to use
  \end{itemize}

\end{frame}

\begin{frame}
  \frametitle{Ambari}

% Integrates with many components. Feasible subset for project?

  \begin{itemize}
    \item Web-based interface
    \item Installation, management and monitoring of Hadoop clusters
    \item Easily integratable with its RESTful APIs
    \item Very wide range of features
  \end{itemize}

\end{frame}

\begin{frame}
  \frametitle{Chukwa}

% Not a particularly good candidate for a project. Builds on storage
% and computation systems. Would be an exercise in programming the logic
% layer without having an actual back-end (which is interesting, but not ideal)

  \begin{itemize}
    \item Data collection
    \item Mostly used for monitoring and analyzing systems
    \item Builds on Storage and Computation systems
    \item Very scalable and robust
  \end{itemize}

\end{frame}

\begin{frame}
  \frametitle{Choosing between the three}
  
  \begin{itemize}
    \item Chukwa
      \only<2>{
      \begin{itemize}
        \item Too heavily based on other systems
        \item Therefore an interesting project, but not the scope that we are looking for
      \end{itemize}}
    \item Ambari
      \only<3>{
      \begin{itemize}
        \item Too large; would have to pick subset of functionality
        \item Choice would again be very dependant on work of other groups
      \end{itemize}}
    \item Zookeeper
      \only<4>{
      \begin{itemize}
        \item Focusses on concurrent programming
        \item Has a core which we can start implementing, and continue from there
        \item Great for a small project like this
      \end{itemize}}
  \end{itemize}

\end{frame}

\begin{frame}
\frametitle{Choice of project}

{\Large \textbf{TimeKeeper} }
  \begin{itemize}
    \item A distributed data store for service orchestration
    \item Focus on consistency, availability, scalability
    \item Not unlike ZooKeeper
  \end{itemize}

  \begin{itemize}
    \item Central component of orchestration
    \item Greate concurrency
    \item Good scope
  \end{itemize}

\end{frame}


\begin{frame}
\frametitle{Project}

\begin{itemize}
  \item Deviation from sequential use: for sequential use, we have roughly @Data.Map@ with change notifications. Requirement: availibility, scalability.
  \item We will implement the core logic of an in memory consistent/available data store. Stages, at least three, for example:
    \begin{itemize}
      \item Serve from one process
      \item Serve through multiple processes: master/worker
      \item Serve more efficiently with multiple processes: \\ implement sharding (how to deal with consistency?)
    \end{itemize}
  \end{itemize}
\end{frame}

\begin{frame}
\frametitle{Experimentation}
  \begin{itemize}
    \item Measure req/s for write-intensive vs. read-intensive workloads
    \item Measure req/s for single point of synchronisation vs. sharded
    \item Measure effect of simulated intercontinental latency on throughput
  \end{itemize}
\end{frame}


\begin{frame}
\frametitle{$\qed$.}
Questions?

\end{frame}
\end{document}
