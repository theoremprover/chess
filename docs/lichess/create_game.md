





<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
  <link rel="dns-prefetch" href="https://assets-cdn.github.com">
  <link rel="dns-prefetch" href="https://avatars0.githubusercontent.com">
  <link rel="dns-prefetch" href="https://avatars1.githubusercontent.com">
  <link rel="dns-prefetch" href="https://avatars2.githubusercontent.com">
  <link rel="dns-prefetch" href="https://avatars3.githubusercontent.com">
  <link rel="dns-prefetch" href="https://github-cloud.s3.amazonaws.com">
  <link rel="dns-prefetch" href="https://user-images.githubusercontent.com/">



  <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/frameworks-bedfc518345498ab3204d330c1727cde7e733526a09cd7df6867f6a231565091.css" integrity="sha256-vt/FGDRUmKsyBNMwwXJ83n5zNSagnNffaGf2ojFWUJE=" media="all" rel="stylesheet" />
  <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/github-63a53e9762b0d5d8d4f52e5ed8855df0850a00402ac6543862a93f50691230c8.css" integrity="sha256-Y6U+l2Kw1djU9S5e2IVd8IUKAEAqxlQ4Yqk/UGkSMMg=" media="all" rel="stylesheet" />
  
  
  <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/site-877643c520258c4fa15ac8d1664d84efd0e3db56f5e544ccac58da0e50489904.css" integrity="sha256-h3ZDxSAljE+hWsjRZk2E79Dj21b15UTMrFjaDlBImQQ=" media="all" rel="stylesheet" />
  

  <meta name="viewport" content="width=device-width">
  
  <title>lila/create_game.md at master · ornicar/lila · GitHub</title>
  <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub">
  <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub">
  <meta property="fb:app_id" content="1401488693436528">

    
    <meta content="https://avatars3.githubusercontent.com/u/140370?v=4&amp;s=400" property="og:image" /><meta content="GitHub" property="og:site_name" /><meta content="object" property="og:type" /><meta content="ornicar/lila" property="og:title" /><meta content="https://github.com/ornicar/lila" property="og:url" /><meta content="lila - lichess.org: the forever free, adless and open source chess server." property="og:description" />

  <link rel="assets" href="https://assets-cdn.github.com/">
  
  <meta name="pjax-timeout" content="1000">
  
  <meta name="request-id" content="D092:1347:26B6556:47D250B:59D0FDD2" data-pjax-transient>
  

  <meta name="selected-link" value="repo_source" data-pjax-transient>

  <meta name="google-site-verification" content="KT5gs8h0wvaagLKAVWq8bbeNwnZZK1r1XQysX3xurLU">
<meta name="google-site-verification" content="ZzhVyEFwb7w3e0-uOTltm8Jsck2F5StVihD0exw2fsA">
    <meta name="google-analytics" content="UA-3769691-2">

<meta content="collector.githubapp.com" name="octolytics-host" /><meta content="github" name="octolytics-app-id" /><meta content="https://collector.githubapp.com/github-external/browser_event" name="octolytics-event-url" /><meta content="D092:1347:26B6556:47D250B:59D0FDD2" name="octolytics-dimension-request_id" /><meta content="iad" name="octolytics-dimension-region_edge" /><meta content="iad" name="octolytics-dimension-region_render" />
<meta content="/&lt;user-name&gt;/&lt;repo-name&gt;/blob/show" data-pjax-transient="true" name="analytics-location" />




  <meta class="js-ga-set" name="dimension1" content="Logged Out">


  

      <meta name="hostname" content="github.com">
  <meta name="user-login" content="">

      <meta name="expected-hostname" content="github.com">
    <meta name="js-proxy-site-detection-payload" content="ZjRiYTJiODNjM2QxNDg1NjYwNzE0MzUwZmQyMjJiZTg2Y2MwY2I3N2Y4ZDZlYzg5NWQ1ZWQwNmY3MzA2ODZkMXx7InJlbW90ZV9hZGRyZXNzIjoiODQuMTUxLjE1NS4xMzgiLCJyZXF1ZXN0X2lkIjoiRDA5MjoxMzQ3OjI2QjY1NTY6NDdEMjUwQjo1OUQwRkREMiIsInRpbWVzdGFtcCI6MTUwNjg2ODY5MCwiaG9zdCI6ImdpdGh1Yi5jb20ifQ==">


  <meta name="html-safe-nonce" content="ae3cb06b474643f2d923efc3f6005ffe6217b0c9">

  <meta http-equiv="x-pjax-version" content="ef1ae5965f157ea6cca26157aaa21ddb">
  

      <link href="https://github.com/ornicar/lila/commits/master.atom" rel="alternate" title="Recent Commits to lila:master" type="application/atom+xml">

  <meta name="description" content="lila - lichess.org: the forever free, adless and open source chess server.">
  <meta name="go-import" content="github.com/ornicar/lila git https://github.com/ornicar/lila.git">

  <meta content="140370" name="octolytics-dimension-user_id" /><meta content="ornicar" name="octolytics-dimension-user_login" /><meta content="3507455" name="octolytics-dimension-repository_id" /><meta content="ornicar/lila" name="octolytics-dimension-repository_nwo" /><meta content="true" name="octolytics-dimension-repository_public" /><meta content="false" name="octolytics-dimension-repository_is_fork" /><meta content="3507455" name="octolytics-dimension-repository_network_root_id" /><meta content="ornicar/lila" name="octolytics-dimension-repository_network_root_nwo" /><meta content="false" name="octolytics-dimension-repository_explore_github_marketplace_ci_cta_shown" />


    <link rel="canonical" href="https://github.com/ornicar/lila/blob/master/doc/mobile/create_game.md" data-pjax-transient>


  <meta name="browser-stats-url" content="https://api.github.com/_private/browser/stats">

  <meta name="browser-errors-url" content="https://api.github.com/_private/browser/errors">

  <link rel="mask-icon" href="https://assets-cdn.github.com/pinned-octocat.svg" color="#000000">
  <link rel="icon" type="image/x-icon" href="https://assets-cdn.github.com/favicon.ico">

<meta name="theme-color" content="#1e2327">



  </head>

  <body class="logged-out env-production page-blob">
    

  <div class="position-relative js-header-wrapper ">
    <a href="#start-of-content" tabindex="1" class="px-2 py-4 show-on-focus js-skip-to-content">Skip to content</a>
    <div id="js-pjax-loader-bar" class="pjax-loader-bar"><div class="progress"></div></div>

    
    
    



        <header class="Header header-logged-out  position-relative f4 py-3" role="banner">
  <div class="container-lg d-flex px-3">
    <div class="d-flex flex-justify-between flex-items-center">
      <a class="header-logo-invertocat my-0" href="https://github.com/" aria-label="Homepage" data-ga-click="(Logged out) Header, go to homepage, icon:logo-wordmark">
        <svg aria-hidden="true" class="octicon octicon-mark-github" height="32" version="1.1" viewBox="0 0 16 16" width="32"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8z"/></svg>
      </a>

    </div>

    <div class="HeaderMenu HeaderMenu--bright d-flex flex-justify-between flex-auto">
        <nav class="mt-0">
          <ul class="d-flex list-style-none">
              <li class="ml-2">
                <a href="/features" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:features" data-selected-links="/features /features/project-management /features/code-review /features/project-management /features/integrations /features">
                  Features
</a>              </li>
              <li class="ml-4">
                <a href="/business" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:business" data-selected-links="/business /business/security /business/customers /business">
                  Business
</a>              </li>

              <li class="ml-4">
                <a href="/explore" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:explore" data-selected-links="/explore /trending /trending/developers /integrations /integrations/feature/code /integrations/feature/collaborate /integrations/feature/ship showcases showcases_search showcases_landing /explore">
                  Explore
</a>              </li>

              <li class="ml-4">
                    <a href="/marketplace" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:marketplace" data-selected-links=" /marketplace">
                      Marketplace
</a>              </li>
              <li class="ml-4">
                <a href="/pricing" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:pricing" data-selected-links="/pricing /pricing/developer /pricing/team /pricing/business-hosted /pricing/business-enterprise /pricing">
                  Pricing
</a>              </li>
          </ul>
        </nav>

      <div class="d-flex">
          <div class="d-lg-flex flex-items-center mr-3">
            <div class="header-search scoped-search site-scoped-search js-site-search" role="search">
  <!-- '"` --><!-- </textarea></xmp> --></option></form><form accept-charset="UTF-8" action="/ornicar/lila/search" class="js-site-search-form" data-scoped-search-url="/ornicar/lila/search" data-unscoped-search-url="/search" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
    <label class="form-control header-search-wrapper js-chromeless-input-container">
        <a href="/ornicar/lila/blob/master/doc/mobile/create_game.md" class="header-search-scope no-underline">This repository</a>
      <input type="text"
        class="form-control header-search-input js-site-search-focus js-site-search-field is-clearable"
        data-hotkey="s"
        name="q"
        value=""
        placeholder="Search"
        aria-label="Search this repository"
        data-unscoped-placeholder="Search GitHub"
        data-scoped-placeholder="Search"
        autocapitalize="off">
        <input type="hidden" class="js-site-search-type-field" name="type" >
    </label>
</form></div>

          </div>

        <span class="d-inline-block">
            <div class="HeaderNavlink px-0 py-2 m-0">
              <a class="text-bold text-white no-underline" href="/login?return_to=%2Fornicar%2Flila%2Fblob%2Fmaster%2Fdoc%2Fmobile%2Fcreate_game.md" data-ga-click="(Logged out) Header, clicked Sign in, text:sign-in">Sign in</a>
                <span class="text-gray">or</span>
                <a class="text-bold text-white no-underline" href="/join?source=header-repo" data-ga-click="(Logged out) Header, clicked Sign up, text:sign-up">Sign up</a>
            </div>
        </span>
      </div>
    </div>
  </div>
</header>


  </div>

  <div id="start-of-content" class="show-on-focus"></div>

    <div id="js-flash-container">
</div>



  <div role="main">
        <div itemscope itemtype="http://schema.org/SoftwareSourceCode">
    <div id="js-repo-pjax-container" data-pjax-container>
      





    <div class="pagehead repohead instapaper_ignore readability-menu experiment-repo-nav">
      <div class="container repohead-details-container">

        <ul class="pagehead-actions">
  <li>
      <a href="/login?return_to=%2Fornicar%2Flila"
    class="btn btn-sm btn-with-count tooltipped tooltipped-n"
    aria-label="You must be signed in to watch a repository" rel="nofollow">
    <svg aria-hidden="true" class="octicon octicon-eye" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M8.06 2C3 2 0 8 0 8s3 6 8.06 6C13 14 16 8 16 8s-3-6-7.94-6zM8 12c-2.2 0-4-1.78-4-4 0-2.2 1.8-4 4-4 2.22 0 4 1.8 4 4 0 2.22-1.78 4-4 4zm2-4c0 1.11-.89 2-2 2-1.11 0-2-.89-2-2 0-1.11.89-2 2-2 1.11 0 2 .89 2 2z"/></svg>
    Watch
  </a>
  <a class="social-count" href="/ornicar/lila/watchers"
     aria-label="200 users are watching this repository">
    200
  </a>

  </li>

  <li>
      <a href="/login?return_to=%2Fornicar%2Flila"
    class="btn btn-sm btn-with-count tooltipped tooltipped-n"
    aria-label="You must be signed in to star a repository" rel="nofollow">
    <svg aria-hidden="true" class="octicon octicon-star" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M14 6l-4.9-.64L7 1 4.9 5.36 0 6l3.6 3.26L2.67 14 7 11.67 11.33 14l-.93-4.74z"/></svg>
    Star
  </a>

    <a class="social-count js-social-count" href="/ornicar/lila/stargazers"
      aria-label="3416 users starred this repository">
      3,416
    </a>

  </li>

  <li>
      <a href="/login?return_to=%2Fornicar%2Flila"
        class="btn btn-sm btn-with-count tooltipped tooltipped-n"
        aria-label="You must be signed in to fork a repository" rel="nofollow">
        <svg aria-hidden="true" class="octicon octicon-repo-forked" height="16" version="1.1" viewBox="0 0 10 16" width="10"><path fill-rule="evenodd" d="M8 1a1.993 1.993 0 0 0-1 3.72V6L5 8 3 6V4.72A1.993 1.993 0 0 0 2 1a1.993 1.993 0 0 0-1 3.72V6.5l3 3v1.78A1.993 1.993 0 0 0 5 15a1.993 1.993 0 0 0 1-3.72V9.5l3-3V4.72A1.993 1.993 0 0 0 8 1zM2 4.2C1.34 4.2.8 3.65.8 3c0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zm3 10c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zm3-10c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2z"/></svg>
        Fork
      </a>

    <a href="/ornicar/lila/network" class="social-count"
       aria-label="579 users forked this repository">
      579
    </a>
  </li>
</ul>

        <h1 class="public ">
  <svg aria-hidden="true" class="octicon octicon-repo" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M4 9H3V8h1v1zm0-3H3v1h1V6zm0-2H3v1h1V4zm0-2H3v1h1V2zm8-1v12c0 .55-.45 1-1 1H6v2l-1.5-1.5L3 16v-2H1c-.55 0-1-.45-1-1V1c0-.55.45-1 1-1h10c.55 0 1 .45 1 1zm-1 10H1v2h2v-1h3v1h5v-2zm0-10H2v9h9V1z"/></svg>
  <span class="author" itemprop="author"><a href="/ornicar" class="url fn" rel="author">ornicar</a></span><!--
--><span class="path-divider">/</span><!--
--><strong itemprop="name"><a href="/ornicar/lila" data-pjax="#js-repo-pjax-container">lila</a></strong>

</h1>

      </div>
      <div class="container">
        
<nav class="reponav js-repo-nav js-sidenav-container-pjax"
     itemscope
     itemtype="http://schema.org/BreadcrumbList"
     role="navigation"
     data-pjax="#js-repo-pjax-container">

  <span itemscope itemtype="http://schema.org/ListItem" itemprop="itemListElement">
    <a href="/ornicar/lila" class="js-selected-navigation-item selected reponav-item" data-hotkey="g c" data-selected-links="repo_source repo_downloads repo_commits repo_releases repo_tags repo_branches /ornicar/lila" itemprop="url">
      <svg aria-hidden="true" class="octicon octicon-code" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M9.5 3L8 4.5 11.5 8 8 11.5 9.5 13 14 8 9.5 3zm-5 0L0 8l4.5 5L6 11.5 2.5 8 6 4.5 4.5 3z"/></svg>
      <span itemprop="name">Code</span>
      <meta itemprop="position" content="1">
</a>  </span>

    <span itemscope itemtype="http://schema.org/ListItem" itemprop="itemListElement">
      <a href="/ornicar/lila/issues" class="js-selected-navigation-item reponav-item" data-hotkey="g i" data-selected-links="repo_issues repo_labels repo_milestones /ornicar/lila/issues" itemprop="url">
        <svg aria-hidden="true" class="octicon octicon-issue-opened" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M7 2.3c3.14 0 5.7 2.56 5.7 5.7s-2.56 5.7-5.7 5.7A5.71 5.71 0 0 1 1.3 8c0-3.14 2.56-5.7 5.7-5.7zM7 1C3.14 1 0 4.14 0 8s3.14 7 7 7 7-3.14 7-7-3.14-7-7-7zm1 3H6v5h2V4zm0 6H6v2h2v-2z"/></svg>
        <span itemprop="name">Issues</span>
        <span class="Counter">406</span>
        <meta itemprop="position" content="2">
</a>    </span>

  <span itemscope itemtype="http://schema.org/ListItem" itemprop="itemListElement">
    <a href="/ornicar/lila/pulls" class="js-selected-navigation-item reponav-item" data-hotkey="g p" data-selected-links="repo_pulls /ornicar/lila/pulls" itemprop="url">
      <svg aria-hidden="true" class="octicon octicon-git-pull-request" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M11 11.28V5c-.03-.78-.34-1.47-.94-2.06C9.46 2.35 8.78 2.03 8 2H7V0L4 3l3 3V4h1c.27.02.48.11.69.31.21.2.3.42.31.69v6.28A1.993 1.993 0 0 0 10 15a1.993 1.993 0 0 0 1-3.72zm-1 2.92c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zM4 3c0-1.11-.89-2-2-2a1.993 1.993 0 0 0-1 3.72v6.56A1.993 1.993 0 0 0 2 15a1.993 1.993 0 0 0 1-3.72V4.72c.59-.34 1-.98 1-1.72zm-.8 10c0 .66-.55 1.2-1.2 1.2-.65 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2zM2 4.2C1.34 4.2.8 3.65.8 3c0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2z"/></svg>
      <span itemprop="name">Pull requests</span>
      <span class="Counter">10</span>
      <meta itemprop="position" content="3">
</a>  </span>

    <a href="/ornicar/lila/projects" class="js-selected-navigation-item reponav-item" data-hotkey="g b" data-selected-links="repo_projects new_repo_project repo_project /ornicar/lila/projects">
      <svg aria-hidden="true" class="octicon octicon-project" height="16" version="1.1" viewBox="0 0 15 16" width="15"><path fill-rule="evenodd" d="M10 12h3V2h-3v10zm-4-2h3V2H6v8zm-4 4h3V2H2v12zm-1 1h13V1H1v14zM14 0H1a1 1 0 0 0-1 1v14a1 1 0 0 0 1 1h13a1 1 0 0 0 1-1V1a1 1 0 0 0-1-1z"/></svg>
      Projects
      <span class="Counter" >3</span>
</a>
    <a href="/ornicar/lila/wiki" class="js-selected-navigation-item reponav-item" data-hotkey="g w" data-selected-links="repo_wiki /ornicar/lila/wiki">
      <svg aria-hidden="true" class="octicon octicon-book" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M3 5h4v1H3V5zm0 3h4V7H3v1zm0 2h4V9H3v1zm11-5h-4v1h4V5zm0 2h-4v1h4V7zm0 2h-4v1h4V9zm2-6v9c0 .55-.45 1-1 1H9.5l-1 1-1-1H2c-.55 0-1-.45-1-1V3c0-.55.45-1 1-1h5.5l1 1 1-1H15c.55 0 1 .45 1 1zm-8 .5L7.5 3H2v9h6V3.5zm7-.5H9.5l-.5.5V12h6V3z"/></svg>
      Wiki
</a>

    <div class="reponav-dropdown js-menu-container">
      <button type="button" class="btn-link reponav-item reponav-dropdown js-menu-target " data-no-toggle aria-expanded="false" aria-haspopup="true">
        Insights
        <svg aria-hidden="true" class="octicon octicon-triangle-down v-align-middle text-gray" height="11" version="1.1" viewBox="0 0 12 16" width="8"><path fill-rule="evenodd" d="M0 5l6 6 6-6z"/></svg>
      </button>
      <div class="dropdown-menu-content js-menu-content">
        <div class="dropdown-menu dropdown-menu-sw">
          <a class="dropdown-item" href="/ornicar/lila/pulse" data-skip-pjax>
            <svg aria-hidden="true" class="octicon octicon-pulse" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M11.5 8L8.8 5.4 6.6 8.5 5.5 1.6 2.38 8H0v2h3.6l.9-1.8.9 5.4L9 8.5l1.6 1.5H14V8z"/></svg>
            Pulse
          </a>
          <a class="dropdown-item" href="/ornicar/lila/graphs" data-skip-pjax>
            <svg aria-hidden="true" class="octicon octicon-graph" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M16 14v1H0V0h1v14h15zM5 13H3V8h2v5zm4 0H7V3h2v10zm4 0h-2V6h2v7z"/></svg>
            Graphs
          </a>
        </div>
      </div>
    </div>
</nav>

      </div>
    </div>

<div class="container new-discussion-timeline experiment-repo-nav">
  <div class="repository-content">

    
  <a href="/ornicar/lila/blob/005e911206fdf45ad3d2f158fc3394f27dd4e665/doc/mobile/create_game.md" class="d-none js-permalink-shortcut" data-hotkey="y">Permalink</a>

  <!-- blob contrib key: blob_contributors:v21:334e5415e24e668fde173bad497e4766 -->

  <div class="file-navigation js-zeroclipboard-container">
    
<div class="select-menu branch-select-menu js-menu-container js-select-menu float-left">
  <button class=" btn btn-sm select-menu-button js-menu-target css-truncate" data-hotkey="w"
    
    type="button" aria-label="Switch branches or tags" aria-expanded="false" aria-haspopup="true">
      <i>Branch:</i>
      <span class="js-select-button css-truncate-target">master</span>
  </button>

  <div class="select-menu-modal-holder js-menu-content js-navigation-container" data-pjax>

    <div class="select-menu-modal">
      <div class="select-menu-header">
        <svg aria-label="Close" class="octicon octicon-x js-menu-close" height="16" role="img" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M7.48 8l3.75 3.75-1.48 1.48L6 9.48l-3.75 3.75-1.48-1.48L4.52 8 .77 4.25l1.48-1.48L6 6.52l3.75-3.75 1.48 1.48z"/></svg>
        <span class="select-menu-title">Switch branches/tags</span>
      </div>

      <div class="select-menu-filters">
        <div class="select-menu-text-filter">
          <input type="text" aria-label="Filter branches/tags" id="context-commitish-filter-field" class="form-control js-filterable-field js-navigation-enable" placeholder="Filter branches/tags">
        </div>
        <div class="select-menu-tabs">
          <ul>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="branches" data-filter-placeholder="Filter branches/tags" class="js-select-menu-tab" role="tab">Branches</a>
            </li>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="tags" data-filter-placeholder="Find a tag…" class="js-select-menu-tab" role="tab">Tags</a>
            </li>
          </ul>
        </div>
      </div>

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="branches" role="menu">

        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/3d-perspective/doc/mobile/create_game.md"
               data-name="3d-perspective"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                3d-perspective
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/Divider/doc/mobile/create_game.md"
               data-name="Divider"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                Divider
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/HorizPGN/doc/mobile/create_game.md"
               data-name="HorizPGN"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                HorizPGN
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/TensorFlow/doc/mobile/create_game.md"
               data-name="TensorFlow"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                TensorFlow
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/activity/doc/mobile/create_game.md"
               data-name="activity"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                activity
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/aiuser/doc/mobile/create_game.md"
               data-name="aiuser"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                aiuser
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/analyse-cg6/doc/mobile/create_game.md"
               data-name="analyse-cg6"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                analyse-cg6
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/analyse-snabbdom/doc/mobile/create_game.md"
               data-name="analyse-snabbdom"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                analyse-snabbdom
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/analysis-full-pgn/doc/mobile/create_game.md"
               data-name="analysis-full-pgn"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                analysis-full-pgn
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/analysis-mithril-bug/doc/mobile/create_game.md"
               data-name="analysis-mithril-bug"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                analysis-mithril-bug
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/analysis-typesafe/doc/mobile/create_game.md"
               data-name="analysis-typesafe"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                analysis-typesafe
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/analysisPremove/doc/mobile/create_game.md"
               data-name="analysisPremove"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                analysisPremove
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/analysismenu2/doc/mobile/create_game.md"
               data-name="analysismenu2"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                analysismenu2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/anand/doc/mobile/create_game.md"
               data-name="anand"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                anand
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/antichess-ai/doc/mobile/create_game.md"
               data-name="antichess-ai"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                antichess-ai
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/applePay/doc/mobile/create_game.md"
               data-name="applePay"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                applePay
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/arex1337-patch-1/doc/mobile/create_game.md"
               data-name="arex1337-patch-1"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                arex1337-patch-1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/async-profile/doc/mobile/create_game.md"
               data-name="async-profile"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                async-profile
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/autocomplete/doc/mobile/create_game.md"
               data-name="autocomplete"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                autocomplete
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/autoformat/doc/mobile/create_game.md"
               data-name="autoformat"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                autoformat
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/badge/doc/mobile/create_game.md"
               data-name="badge"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                badge
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/bcryptPasswords/doc/mobile/create_game.md"
               data-name="bcryptPasswords"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                bcryptPasswords
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/betterClockCounting/doc/mobile/create_game.md"
               data-name="betterClockCounting"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                betterClockCounting
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/blurEnhance/doc/mobile/create_game.md"
               data-name="blurEnhance"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                blurEnhance
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/build-artifacts/doc/mobile/create_game.md"
               data-name="build-artifacts"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                build-artifacts
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/cachedresolution/doc/mobile/create_game.md"
               data-name="cachedresolution"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                cachedresolution
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/cash/doc/mobile/create_game.md"
               data-name="cash"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                cash
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/centis-utils/doc/mobile/create_game.md"
               data-name="centis-utils"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                centis-utils
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ceval-multipv-panel/doc/mobile/create_game.md"
               data-name="ceval-multipv-panel"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ceval-multipv-panel
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ceval-multipv/doc/mobile/create_game.md"
               data-name="ceval-multipv"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ceval-multipv
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ceval-pnacl/doc/mobile/create_game.md"
               data-name="ceval-pnacl"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ceval-pnacl
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ceval-pvs/doc/mobile/create_game.md"
               data-name="ceval-pvs"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ceval-pvs
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/challangeViewLag/doc/mobile/create_game.md"
               data-name="challangeViewLag"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                challangeViewLag
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/challenge-granter/doc/mobile/create_game.md"
               data-name="challenge-granter"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                challenge-granter
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/challenge2/doc/mobile/create_game.md"
               data-name="challenge2"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                challenge2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/change-blind-mode/doc/mobile/create_game.md"
               data-name="change-blind-mode"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                change-blind-mode
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/chat/doc/mobile/create_game.md"
               data-name="chat"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                chat
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/chat2/doc/mobile/create_game.md"
               data-name="chat2"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                chat2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/clarkerubber-lower-learn-rate/doc/mobile/create_game.md"
               data-name="clarkerubber-lower-learn-rate"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                clarkerubber-lower-learn-rate
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/clarkerubber-patch-1/doc/mobile/create_game.md"
               data-name="clarkerubber-patch-1"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                clarkerubber-patch-1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/clarkerubber-patch-2/doc/mobile/create_game.md"
               data-name="clarkerubber-patch-2"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                clarkerubber-patch-2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/clarkerubber-patch-3/doc/mobile/create_game.md"
               data-name="clarkerubber-patch-3"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                clarkerubber-patch-3
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/clarkeyTensorFlow/doc/mobile/create_game.md"
               data-name="clarkeyTensorFlow"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                clarkeyTensorFlow
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/clockEnhance/doc/mobile/create_game.md"
               data-name="clockEnhance"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                clockEnhance
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/coach/doc/mobile/create_game.md"
               data-name="coach"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                coach
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/coach2/doc/mobile/create_game.md"
               data-name="coach2"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                coach2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/configurable-streamers/doc/mobile/create_game.md"
               data-name="configurable-streamers"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                configurable-streamers
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/corresp-email/doc/mobile/create_game.md"
               data-name="corresp-email"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                corresp-email
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/crazyhouse-ai/doc/mobile/create_game.md"
               data-name="crazyhouse-ai"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                crazyhouse-ai
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/crisis/doc/mobile/create_game.md"
               data-name="crisis"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                crisis
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/crowdin-no-subdomain/doc/mobile/create_game.md"
               data-name="crowdin-no-subdomain"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                crowdin-no-subdomain
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/crowdin-pre-xml/doc/mobile/create_game.md"
               data-name="crowdin-pre-xml"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                crowdin-pre-xml
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/crowdin/doc/mobile/create_game.md"
               data-name="crowdin"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                crowdin
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/custom-background/doc/mobile/create_game.md"
               data-name="custom-background"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                custom-background
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/dark-texture/doc/mobile/create_game.md"
               data-name="dark-texture"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                dark-texture
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/dasher/doc/mobile/create_game.md"
               data-name="dasher"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                dasher
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/default-bg/doc/mobile/create_game.md"
               data-name="default-bg"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                default-bg
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/disable-unjoinable-buttons/doc/mobile/create_game.md"
               data-name="disable-unjoinable-buttons"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                disable-unjoinable-buttons
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/disaster/doc/mobile/create_game.md"
               data-name="disaster"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                disaster
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/dominated-viewport/doc/mobile/create_game.md"
               data-name="dominated-viewport"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                dominated-viewport
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/dropWSBC/doc/mobile/create_game.md"
               data-name="dropWSBC"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                dropWSBC
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/durationlocale/doc/mobile/create_game.md"
               data-name="durationlocale"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                durationlocale
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/editor-cg6/doc/mobile/create_game.md"
               data-name="editor-cg6"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                editor-cg6
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/email-confirmaction/doc/mobile/create_game.md"
               data-name="email-confirmaction"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                email-confirmaction
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/embedTvChannel/doc/mobile/create_game.md"
               data-name="embedTvChannel"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                embedTvChannel
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/eval-cache-v/doc/mobile/create_game.md"
               data-name="eval-cache-v"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                eval-cache-v
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/eval-cache-variants/doc/mobile/create_game.md"
               data-name="eval-cache-variants"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                eval-cache-variants
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/eval-cache/doc/mobile/create_game.md"
               data-name="eval-cache"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                eval-cache
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/event/doc/mobile/create_game.md"
               data-name="event"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                event
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ext-zen/doc/mobile/create_game.md"
               data-name="ext-zen"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ext-zen
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/fancier-tournament-schedule/doc/mobile/create_game.md"
               data-name="fancier-tournament-schedule"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                fancier-tournament-schedule
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/fast-drag/doc/mobile/create_game.md"
               data-name="fast-drag"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                fast-drag
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/fishnet-cache/doc/mobile/create_game.md"
               data-name="fishnet-cache"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                fishnet-cache
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/fishnet-crazyhouse/doc/mobile/create_game.md"
               data-name="fishnet-crazyhouse"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                fishnet-crazyhouse
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/fishnet-storage/doc/mobile/create_game.md"
               data-name="fishnet-storage"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                fishnet-storage
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/fishnet/doc/mobile/create_game.md"
               data-name="fishnet"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                fishnet
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/fix-vagrant-instructions/doc/mobile/create_game.md"
               data-name="fix-vagrant-instructions"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                fix-vagrant-instructions
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/gamebook/doc/mobile/create_game.md"
               data-name="gamebook"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                gamebook
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/hackfix-3d-zoom/doc/mobile/create_game.md"
               data-name="hackfix-3d-zoom"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                hackfix-3d-zoom
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/hideExtraTitle/doc/mobile/create_game.md"
               data-name="hideExtraTitle"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                hideExtraTitle
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/horizontal/doc/mobile/create_game.md"
               data-name="horizontal"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                horizontal
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/hyperlinked-message/doc/mobile/create_game.md"
               data-name="hyperlinked-message"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                hyperlinked-message
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/icc-ip-api/doc/mobile/create_game.md"
               data-name="icc-ip-api"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                icc-ip-api
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/improve-modzone/doc/mobile/create_game.md"
               data-name="improve-modzone"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                improve-modzone
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ingame-public-chat/doc/mobile/create_game.md"
               data-name="ingame-public-chat"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ingame-public-chat
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/inquiry-history/doc/mobile/create_game.md"
               data-name="inquiry-history"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                inquiry-history
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/insight-period/doc/mobile/create_game.md"
               data-name="insight-period"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                insight-period
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/insight-time/doc/mobile/create_game.md"
               data-name="insight-time"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                insight-time
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/insights/doc/mobile/create_game.md"
               data-name="insights"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                insights
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ios-push-2/doc/mobile/create_game.md"
               data-name="ios-push-2"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ios-push-2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ios-push-legacy-grrr/doc/mobile/create_game.md"
               data-name="ios-push-legacy-grrr"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ios-push-legacy-grrr
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ios-push-rm-cursor-fishnet/doc/mobile/create_game.md"
               data-name="ios-push-rm-cursor-fishnet"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ios-push-rm-cursor-fishnet
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ios-push-rm-cursor/doc/mobile/create_game.md"
               data-name="ios-push-rm-cursor"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ios-push-rm-cursor
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ios-push/doc/mobile/create_game.md"
               data-name="ios-push"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ios-push
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ipv6/doc/mobile/create_game.md"
               data-name="ipv6"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ipv6
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/irwin-requests/doc/mobile/create_game.md"
               data-name="irwin-requests"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                irwin-requests
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/isaacl-patch-1/doc/mobile/create_game.md"
               data-name="isaacl-patch-1"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                isaacl-patch-1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/kamon-and-RM12/doc/mobile/create_game.md"
               data-name="kamon-and-RM12"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                kamon-and-RM12
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/kamon-only/doc/mobile/create_game.md"
               data-name="kamon-only"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                kamon-only
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/kamon/doc/mobile/create_game.md"
               data-name="kamon"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                kamon
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/keyboard-move-autocomplete/doc/mobile/create_game.md"
               data-name="keyboard-move-autocomplete"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                keyboard-move-autocomplete
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/killPRM/doc/mobile/create_game.md"
               data-name="killPRM"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                killPRM
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/l10n_master/doc/mobile/create_game.md"
               data-name="l10n_master"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                l10n_master
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/literate-moves/doc/mobile/create_game.md"
               data-name="literate-moves"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                literate-moves
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/liveval/doc/mobile/create_game.md"
               data-name="liveval"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                liveval
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/mapdb/doc/mobile/create_game.md"
               data-name="mapdb"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                mapdb
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open selected"
               href="/ornicar/lila/blob/master/doc/mobile/create_game.md"
               data-name="master"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                master
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/mention_notif_fix/doc/mobile/create_game.md"
               data-name="mention_notif_fix"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                mention_notif_fix
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/minorGameCleanup/doc/mobile/create_game.md"
               data-name="minorGameCleanup"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                minorGameCleanup
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/mithril-play-sweet/doc/mobile/create_game.md"
               data-name="mithril-play-sweet"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                mithril-play-sweet
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/mithril-seeks/doc/mobile/create_game.md"
               data-name="mithril-seeks"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                mithril-seeks
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/mithril1/doc/mobile/create_game.md"
               data-name="mithril1"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                mithril1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/mithrilmaster/doc/mobile/create_game.md"
               data-name="mithrilmaster"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                mithrilmaster
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/mixed-cache-boot/doc/mobile/create_game.md"
               data-name="mixed-cache-boot"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                mixed-cache-boot
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/mobile-signup-email-confirmation/doc/mobile/create_game.md"
               data-name="mobile-signup-email-confirmation"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                mobile-signup-email-confirmation
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/move-no-read/doc/mobile/create_game.md"
               data-name="move-no-read"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                move-no-read
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/moveLat/doc/mobile/create_game.md"
               data-name="moveLat"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                moveLat
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/multipool/doc/mobile/create_game.md"
               data-name="multipool"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                multipool
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/multipv/doc/mobile/create_game.md"
               data-name="multipv"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                multipv
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/new-cache/doc/mobile/create_game.md"
               data-name="new-cache"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                new-cache
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/new-search/doc/mobile/create_game.md"
               data-name="new-search"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                new-search
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/no-momentjs/doc/mobile/create_game.md"
               data-name="no-momentjs"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                no-momentjs
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/noMongoCache/doc/mobile/create_game.md"
               data-name="noMongoCache"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                noMongoCache
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/nosizzle/doc/mobile/create_game.md"
               data-name="nosizzle"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                nosizzle
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/onesignal-service-worker/doc/mobile/create_game.md"
               data-name="onesignal-service-worker"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                onesignal-service-worker
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/onesignal/doc/mobile/create_game.md"
               data-name="onesignal"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                onesignal
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/patch-youtube-links/doc/mobile/create_game.md"
               data-name="patch-youtube-links"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                patch-youtube-links
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/perfStats/doc/mobile/create_game.md"
               data-name="perfStats"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                perfStats
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/play24/doc/mobile/create_game.md"
               data-name="play24"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                play24
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/play25-ws/doc/mobile/create_game.md"
               data-name="play25-ws"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                play25-ws
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/play25/doc/mobile/create_game.md"
               data-name="play25"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                play25
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/polyglot-syzygy/doc/mobile/create_game.md"
               data-name="polyglot-syzygy"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                polyglot-syzygy
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/polyglot/doc/mobile/create_game.md"
               data-name="polyglot"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                polyglot
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/pool/doc/mobile/create_game.md"
               data-name="pool"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                pool
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/practice-feedback/doc/mobile/create_game.md"
               data-name="practice-feedback"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                practice-feedback
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/practice/doc/mobile/create_game.md"
               data-name="practice"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                practice
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/pre-scala-212/doc/mobile/create_game.md"
               data-name="pre-scala-212"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                pre-scala-212
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/profile_links/doc/mobile/create_game.md"
               data-name="profile_links"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                profile_links
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/proxy-selector/doc/mobile/create_game.md"
               data-name="proxy-selector"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                proxy-selector
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/pub_chat_mod/doc/mobile/create_game.md"
               data-name="pub_chat_mod"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                pub_chat_mod
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/push/doc/mobile/create_game.md"
               data-name="push"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                push
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/puzzle-cg6/doc/mobile/create_game.md"
               data-name="puzzle-cg6"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                puzzle-cg6
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/puzzle-tags/doc/mobile/create_game.md"
               data-name="puzzle-tags"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                puzzle-tags
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/puzzle-tour/doc/mobile/create_game.md"
               data-name="puzzle-tour"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                puzzle-tour
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/puzzle-ui/doc/mobile/create_game.md"
               data-name="puzzle-ui"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                puzzle-ui
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/puzzle2-ipv6/doc/mobile/create_game.md"
               data-name="puzzle2-ipv6"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                puzzle2-ipv6
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/puzzle2/doc/mobile/create_game.md"
               data-name="puzzle2"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                puzzle2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/recover-ceval-crash/doc/mobile/create_game.md"
               data-name="recover-ceval-crash"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                recover-ceval-crash
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/relay-block/doc/mobile/create_game.md"
               data-name="relay-block"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                relay-block
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/relay/doc/mobile/create_game.md"
               data-name="relay"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                relay
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/relay2/doc/mobile/create_game.md"
               data-name="relay2"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                relay2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/reparsing/doc/mobile/create_game.md"
               data-name="reparsing"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                reparsing
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/report-filtering/doc/mobile/create_game.md"
               data-name="report-filtering"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                report-filtering
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/result_overlay/doc/mobile/create_game.md"
               data-name="result_overlay"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                result_overlay
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/retrospect/doc/mobile/create_game.md"
               data-name="retrospect"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                retrospect
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/reuse-chat-snabbdom-init/doc/mobile/create_game.md"
               data-name="reuse-chat-snabbdom-init"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                reuse-chat-snabbdom-init
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/rm-0.12.1/doc/mobile/create_game.md"
               data-name="rm-0.12.1"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                rm-0.12.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/rm-readpref/doc/mobile/create_game.md"
               data-name="rm-readpref"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                rm-readpref
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/rm012/doc/mobile/create_game.md"
               data-name="rm012"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                rm012
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/round-cg6/doc/mobile/create_game.md"
               data-name="round-cg6"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                round-cg6
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/round-timer/doc/mobile/create_game.md"
               data-name="round-timer"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                round-timer
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/round2/doc/mobile/create_game.md"
               data-name="round2"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                round2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/sbtSpeedup/doc/mobile/create_game.md"
               data-name="sbtSpeedup"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                sbtSpeedup
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/scala-2.12-play-2.6/doc/mobile/create_game.md"
               data-name="scala-2.12-play-2.6"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                scala-2.12-play-2.6
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/scaleLagComp/doc/mobile/create_game.md"
               data-name="scaleLagComp"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                scaleLagComp
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/script-async/doc/mobile/create_game.md"
               data-name="script-async"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                script-async
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/service-worker/doc/mobile/create_game.md"
               data-name="service-worker"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                service-worker
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/setClockOnMove/doc/mobile/create_game.md"
               data-name="setClockOnMove"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                setClockOnMove
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/showPingInPowertip/doc/mobile/create_game.md"
               data-name="showPingInPowertip"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                showPingInPowertip
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/simulTourney/doc/mobile/create_game.md"
               data-name="simulTourney"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                simulTourney
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/social-login/doc/mobile/create_game.md"
               data-name="social-login"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                social-login
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/social-share/doc/mobile/create_game.md"
               data-name="social-share"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                social-share
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/stage/doc/mobile/create_game.md"
               data-name="stage"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                stage
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/standardOverlay/doc/mobile/create_game.md"
               data-name="standardOverlay"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                standardOverlay
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/studies-heading-grammar-fix/doc/mobile/create_game.md"
               data-name="studies-heading-grammar-fix"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                studies-heading-grammar-fix
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/study-editable-tags/doc/mobile/create_game.md"
               data-name="study-editable-tags"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                study-editable-tags
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/study-embed/doc/mobile/create_game.md"
               data-name="study-embed"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                study-embed
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/study-explorer-insert/doc/mobile/create_game.md"
               data-name="study-explorer-insert"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                study-explorer-insert
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/study-friend-list/doc/mobile/create_game.md"
               data-name="study-friend-list"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                study-friend-list
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/study-practice/doc/mobile/create_game.md"
               data-name="study-practice"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                study-practice
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/study-sticky/doc/mobile/create_game.md"
               data-name="study-sticky"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                study-sticky
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/study-url-ply/doc/mobile/create_game.md"
               data-name="study-url-ply"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                study-url-ply
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/studyInviteSetting/doc/mobile/create_game.md"
               data-name="studyInviteSetting"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                studyInviteSetting
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/submit-unlimited/doc/mobile/create_game.md"
               data-name="submit-unlimited"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                submit-unlimited
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/testRevertPing/doc/mobile/create_game.md"
               data-name="testRevertPing"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                testRevertPing
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/thottleCevalMore/doc/mobile/create_game.md"
               data-name="thottleCevalMore"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                thottleCevalMore
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/title-highlighting-all/doc/mobile/create_game.md"
               data-name="title-highlighting-all"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                title-highlighting-all
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/tournament-pairing-countdown/doc/mobile/create_game.md"
               data-name="tournament-pairing-countdown"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                tournament-pairing-countdown
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/tournamentLeader/doc/mobile/create_game.md"
               data-name="tournamentLeader"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                tournamentLeader
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/tournamentPerf/doc/mobile/create_game.md"
               data-name="tournamentPerf"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                tournamentPerf
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/tournamentStats/doc/mobile/create_game.md"
               data-name="tournamentStats"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                tournamentStats
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/tourney-throttler/doc/mobile/create_game.md"
               data-name="tourney-throttler"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                tourney-throttler
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/tourney-winners/doc/mobile/create_game.md"
               data-name="tourney-winners"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                tourney-winners
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/trace/doc/mobile/create_game.md"
               data-name="trace"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                trace
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/trans-user-side/doc/mobile/create_game.md"
               data-name="trans-user-side"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                trans-user-side
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/travis-yarn/doc/mobile/create_game.md"
               data-name="travis-yarn"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                travis-yarn
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/travisFlakinessExperiments/doc/mobile/create_game.md"
               data-name="travisFlakinessExperiments"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                travisFlakinessExperiments
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/typesafeRound/doc/mobile/create_game.md"
               data-name="typesafeRound"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                typesafeRound
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ub/doc/mobile/create_game.md"
               data-name="ub"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ub
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/ui/site/doc/mobile/create_game.md"
               data-name="ui/site"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                ui/site
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/unifiedOpenings/doc/mobile/create_game.md"
               data-name="unifiedOpenings"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                unifiedOpenings
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/unlink-for-kids/doc/mobile/create_game.md"
               data-name="unlink-for-kids"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                unlink-for-kids
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/unmoved-rooks/doc/mobile/create_game.md"
               data-name="unmoved-rooks"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                unmoved-rooks
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/untitledFriends/doc/mobile/create_game.md"
               data-name="untitledFriends"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                untitledFriends
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/updateTournies/doc/mobile/create_game.md"
               data-name="updateTournies"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                updateTournies
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/userTv/doc/mobile/create_game.md"
               data-name="userTv"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                userTv
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/vagrant-fail-early/doc/mobile/create_game.md"
               data-name="vagrant-fail-early"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                vagrant-fail-early
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/videochat/doc/mobile/create_game.md"
               data-name="videochat"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                videochat
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/worldMap2/doc/mobile/create_game.md"
               data-name="worldMap2"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                worldMap2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/worldmap-pan-zoom/doc/mobile/create_game.md"
               data-name="worldmap-pan-zoom"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                worldmap-pan-zoom
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/yarn-0.26/doc/mobile/create_game.md"
               data-name="yarn-0.26"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                yarn-0.26
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/yarn-rc/doc/mobile/create_game.md"
               data-name="yarn-rc"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                yarn-rc
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/ornicar/lila/blob/zepto/doc/mobile/create_game.md"
               data-name="zepto"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                zepto
              </span>
            </a>
        </div>

          <div class="select-menu-no-results">Nothing to show</div>
      </div>

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="tags">
        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/ornicar/lila/tree/v1.1.0/doc/mobile/create_game.md"
              data-name="v1.1.0"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="v1.1.0">
                v1.1.0
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/ornicar/lila/tree/v1.0.0/doc/mobile/create_game.md"
              data-name="v1.0.0"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="v1.0.0">
                v1.0.0
              </span>
            </a>
        </div>

        <div class="select-menu-no-results">Nothing to show</div>
      </div>

    </div>
  </div>
</div>

    <div class="BtnGroup float-right">
      <a href="/ornicar/lila/find/master"
            class="js-pjax-capture-input btn btn-sm BtnGroup-item"
            data-pjax
            data-hotkey="t">
        Find file
      </a>
      <button aria-label="Copy file path to clipboard" class="js-zeroclipboard btn btn-sm BtnGroup-item tooltipped tooltipped-s" data-copied-hint="Copied!" type="button">Copy path</button>
    </div>
    <div class="breadcrumb js-zeroclipboard-target">
      <span class="repo-root js-repo-root"><span class="js-path-segment"><a href="/ornicar/lila"><span>lila</span></a></span></span><span class="separator">/</span><span class="js-path-segment"><a href="/ornicar/lila/tree/master/doc"><span>doc</span></a></span><span class="separator">/</span><span class="js-path-segment"><a href="/ornicar/lila/tree/master/doc/mobile"><span>mobile</span></a></span><span class="separator">/</span><strong class="final-path">create_game.md</strong>
    </div>
  </div>


  
  <div class="commit-tease">
      <span class="float-right">
        <a class="commit-tease-sha" href="/ornicar/lila/commit/cb06417d169d09c33cfb356175c5575959698e56" data-pjax>
          cb06417
        </a>
        <relative-time datetime="2017-05-26T17:19:32Z">May 26, 2017</relative-time>
      </span>
      <div>
        <img alt="@ornicar" class="avatar" height="20" src="https://avatars0.githubusercontent.com/u/140370?v=4&amp;s=40" width="20" />
        <a href="/ornicar" class="user-mention" rel="author">ornicar</a>
          <a href="/ornicar/lila/commit/cb06417d169d09c33cfb356175c5575959698e56" class="message" data-pjax="true" title="remove hardcoded subdomains">remove hardcoded subdomains</a>
      </div>

    <div class="commit-tease-contributors">
      <button type="button" class="btn-link muted-link contributors-toggle" data-facebox="#blob_contributors_box">
        <strong>1</strong>
         contributor
      </button>
      
    </div>

    <div id="blob_contributors_box" style="display:none">
      <h2 class="facebox-header" data-facebox-id="facebox-header">Users who have contributed to this file</h2>
      <ul class="facebox-user-list" data-facebox-id="facebox-description">
          <li class="facebox-user-list-item">
            <img alt="@ornicar" height="24" src="https://avatars2.githubusercontent.com/u/140370?v=4&amp;s=48" width="24" />
            <a href="/ornicar">ornicar</a>
          </li>
      </ul>
    </div>
  </div>


  <div class="file">
    <div class="file-header">
  <div class="file-actions">

    <div class="BtnGroup">
      <a href="/ornicar/lila/raw/master/doc/mobile/create_game.md" class="btn btn-sm BtnGroup-item" id="raw-url">Raw</a>
        <a href="/ornicar/lila/blame/master/doc/mobile/create_game.md" class="btn btn-sm js-update-url-with-hash BtnGroup-item" data-hotkey="b">Blame</a>
      <a href="/ornicar/lila/commits/master/doc/mobile/create_game.md" class="btn btn-sm BtnGroup-item" rel="nofollow">History</a>
    </div>

        <a class="btn-octicon tooltipped tooltipped-nw"
           href="https://desktop.github.com"
           aria-label="Open this file in GitHub Desktop"
           data-ga-click="Repository, open with desktop, type:windows">
            <svg aria-hidden="true" class="octicon octicon-device-desktop" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M15 2H1c-.55 0-1 .45-1 1v9c0 .55.45 1 1 1h5.34c-.25.61-.86 1.39-2.34 2h8c-1.48-.61-2.09-1.39-2.34-2H15c.55 0 1-.45 1-1V3c0-.55-.45-1-1-1zm0 9H1V3h14v8z"/></svg>
        </a>

        <button type="button" class="btn-octicon disabled tooltipped tooltipped-nw"
          aria-label="You must be signed in to make or propose changes">
          <svg aria-hidden="true" class="octicon octicon-pencil" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M0 12v3h3l8-8-3-3-8 8zm3 2H1v-2h1v1h1v1zm10.3-9.3L12 6 9 3l1.3-1.3a.996.996 0 0 1 1.41 0l1.59 1.59c.39.39.39 1.02 0 1.41z"/></svg>
        </button>
        <button type="button" class="btn-octicon btn-octicon-danger disabled tooltipped tooltipped-nw"
          aria-label="You must be signed in to make or propose changes">
          <svg aria-hidden="true" class="octicon octicon-trashcan" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M11 2H9c0-.55-.45-1-1-1H5c-.55 0-1 .45-1 1H2c-.55 0-1 .45-1 1v1c0 .55.45 1 1 1v9c0 .55.45 1 1 1h7c.55 0 1-.45 1-1V5c.55 0 1-.45 1-1V3c0-.55-.45-1-1-1zm-1 12H3V5h1v8h1V5h1v8h1V5h1v8h1V5h1v9zm1-10H2V3h9v1z"/></svg>
        </button>
  </div>

  <div class="file-info">
      144 lines (106 sloc)
      <span class="file-info-divider"></span>
    3.53 KB
  </div>
</div>

    
  <div id="readme" class="readme blob instapaper_body">
    <article class="markdown-body entry-content" itemprop="text"><h1><a href="#create-a-game" aria-hidden="true" class="anchor" id="user-content-create-a-game"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Create a game</h1>
<h2><a href="#with-ai" aria-hidden="true" class="anchor" id="user-content-with-ai"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>With A.I.</h2>
<div class="highlight highlight-source-shell"><pre>http --form POST l.org/setup/ai variant=1 clock=false time=60 increment=60 level=3 color=random <span class="pl-s"><span class="pl-pds">'</span>Accept:application/vnd.lichess.v1+json<span class="pl-pds">'</span></span></pre></div>
<ul>
<li>level: 1 to 8</li>
<li>color: white | black | random</li>
<li>variant: 1 (standard) | 2 (chess960) | 3 (from position) | 4 (KotH) | 5 (three-check)</li>
<li>fen: if variant is 3, any valid FEN string</li>
</ul>
<p>Response: <code>201 CREATED</code></p>
<div class="highlight highlight-source-js"><pre>{
  <span class="pl-c"><span class="pl-c">//</span> see document format in the play.md doc file</span>
}</pre></div>
<h2><a href="#seek-for-human-opponent" aria-hidden="true" class="anchor" id="user-content-seek-for-human-opponent"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Seek for human opponent</h2>
<p>First you need to connect to the lobby websocket, from which you'll receive the game creation event.</p>
<div class="highlight highlight-source-js"><pre><span class="pl-k">var</span> clientId <span class="pl-k">=</span> <span class="pl-c1">Math</span>.<span class="pl-c1">random</span>().<span class="pl-c1">toString</span>(<span class="pl-c1">36</span>).<span class="pl-c1">substring</span>(<span class="pl-c1">2</span>); <span class="pl-c"><span class="pl-c">//</span> created and stored by the client</span>
<span class="pl-k">var</span> socketVersion <span class="pl-k">=</span> <span class="pl-c1">0</span>; <span class="pl-c"><span class="pl-c">//</span> last message version number seen on this socket. Starts at zero.</span>

<span class="pl-k">var</span> socketUrl <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">'</span>http://socket.l.org:9021/lobby/socket?mobile=1&amp;sri=<span class="pl-pds">'</span></span> <span class="pl-k">+</span> clientId <span class="pl-k">+</span> <span class="pl-s"><span class="pl-pds">'</span>&amp;version=<span class="pl-pds">'</span></span> <span class="pl-k">+</span> socketVersion;

<span class="pl-k">var</span> socket <span class="pl-k">=</span> <span class="pl-k">new</span> <span class="pl-en">WebSocket</span>(socketUrl);</pre></div>
<p>Once connected, you can send seeks over HTTP, using the same clientId</p>
<div class="highlight highlight-source-shell"><pre>http --form POST l.org/setup/hook/{clientId} variant=1 clock=false time=60 increment=60 mode=casual <span class="pl-s"><span class="pl-pds">'</span>Accept:application/vnd.lichess.v1+json<span class="pl-pds">'</span></span></pre></div>
<ul>
<li>clientId: same random ID created by the client and used to connect to the lobby websocket</li>
<li>variant: 1 (standard) | 2 (chess960) | 3 (from position) | 4 (KotH)</li>
<li>mode: casual | rated</li>
</ul>
<p>Response: <code>200 OK</code></p>
<pre><code>ok
</code></pre>
<p>Now you're waiting for someone to accept the seek. The response will come as a socket message:</p>
<div class="highlight highlight-source-js"><pre><span class="pl-c"><span class="pl-c">//</span> the seek was accepted</span>
{
  <span class="pl-s"><span class="pl-pds">"</span>t<span class="pl-pds">"</span></span><span class="pl-k">:</span> <span class="pl-s"><span class="pl-pds">"</span>redirect<span class="pl-pds">"</span></span>, <span class="pl-c"><span class="pl-c">//</span> means we should move on to the game</span>
  <span class="pl-s"><span class="pl-pds">"</span>id<span class="pl-pds">"</span></span><span class="pl-k">:</span> <span class="pl-s"><span class="pl-pds">"</span>abcdefgh1234<span class="pl-pds">"</span></span>
}</pre></div>
<h2><a href="#challenge-someone" aria-hidden="true" class="anchor" id="user-content-challenge-someone"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Challenge someone</h2>
<div class="highlight highlight-source-shell"><pre>http --form POST l.org/setup/friend<span class="pl-k">?</span>user=usernameOrId variant=1 clock=false time=60 increment=60 color=random <span class="pl-s"><span class="pl-pds">'</span>Accept:application/vnd.lichess.v1+json<span class="pl-pds">'</span></span></pre></div>
<ul>
<li>color: white | black | random</li>
<li>variant: 1 (standard) | 2 (chess960) | 3 (from position) | 4 (KotH) | 5 (three-check)</li>
<li>fen: if variant is 3, any valid FEN string</li>
</ul>
<p>Response: <code>201 CREATED</code></p>
<div class="highlight highlight-source-js"><pre>{
  <span class="pl-c"><span class="pl-c">//</span> see document format in the play.md doc file</span>
}</pre></div>
<p>Then, connect to the game websocket as a player (see Play documentation).
Now you must send a message over the socket every 1,5 seconds to keep the challenge open:</p>
<div class="highlight highlight-source-js"><pre><span class="pl-c"><span class="pl-c">//</span> send</span>
{t<span class="pl-k">:</span> <span class="pl-s"><span class="pl-pds">'</span>challenge<span class="pl-pds">'</span></span>, d<span class="pl-k">:</span> <span class="pl-s"><span class="pl-pds">'</span>userIdOfChallengedPlayer<span class="pl-pds">'</span></span>}</pre></div>
<h3><a href="#cancel-the-challenge" aria-hidden="true" class="anchor" id="user-content-cancel-the-challenge"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Cancel the challenge</h3>
<p>A challenge can be aborted before it is accepted, using a HTTP request:</p>
<div class="highlight highlight-source-shell"><pre>http --form GET l.org/<span class="pl-k">&lt;</span>fullGameId<span class="pl-k">&gt;</span>/cancel <span class="pl-s"><span class="pl-pds">'</span>Accept:application/vnd.lichess.v1+json<span class="pl-pds">'</span></span></pre></div>
<h3><a href="#accepted-by-the-opponent" aria-hidden="true" class="anchor" id="user-content-accepted-by-the-opponent"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Accepted by the opponent</h3>
<p>When the challenge is accepted, you will receive a redirect message through the websocket:</p>
<div class="highlight highlight-source-js"><pre><span class="pl-c"><span class="pl-c">//</span> receive</span>
{
  <span class="pl-s"><span class="pl-pds">"</span>t<span class="pl-pds">"</span></span><span class="pl-k">:</span> <span class="pl-s"><span class="pl-pds">"</span>redirect<span class="pl-pds">"</span></span>, <span class="pl-c"><span class="pl-c">//</span> means we should move on to the game</span>
  <span class="pl-s"><span class="pl-pds">"</span>id<span class="pl-pds">"</span></span><span class="pl-k">:</span> <span class="pl-s"><span class="pl-pds">"</span>abcdefgh1234<span class="pl-pds">"</span></span>
}</pre></div>
<h3><a href="#declined-by-the-opponent" aria-hidden="true" class="anchor" id="user-content-declined-by-the-opponent"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Declined by the opponent</h3>
<p>When the challenge is decline, you will receive a message through the websocket:</p>
<div class="highlight highlight-source-js"><pre><span class="pl-c"><span class="pl-c">//</span> receive</span>
{<span class="pl-s"><span class="pl-pds">"</span>t<span class="pl-pds">"</span></span><span class="pl-k">:</span> <span class="pl-s"><span class="pl-pds">"</span>declined<span class="pl-pds">"</span></span>}</pre></div>
<h2><a href="#receive-a-challenge" aria-hidden="true" class="anchor" id="user-content-receive-a-challenge"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Receive a challenge</h2>
<p>Listen for this message on any websocket:</p>
<div class="highlight highlight-source-js"><pre><span class="pl-c"><span class="pl-c">//</span> receive</span>
{
  <span class="pl-s"><span class="pl-pds">"</span>t<span class="pl-pds">"</span></span><span class="pl-k">:</span> <span class="pl-s"><span class="pl-pds">"</span>challengeReminder<span class="pl-pds">"</span></span>,
  <span class="pl-s"><span class="pl-pds">"</span>id<span class="pl-pds">"</span></span><span class="pl-k">:</span> <span class="pl-s"><span class="pl-pds">"</span>abcdefgh<span class="pl-pds">"</span></span>
}</pre></div>
<p>You will receive this every 1,5 seconds aproximatively, until the challenge creator stops sending it.
The challenge ID is also the game public ID.</p>
<h3><a href="#fetch-game-information" aria-hidden="true" class="anchor" id="user-content-fetch-game-information"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Fetch game information</h3>
<p>The first time you see a challenge ID, use it to fetch information about the game:</p>
<div class="highlight highlight-source-shell"><pre>http GET l.org/39b12Ikl <span class="pl-s"><span class="pl-pds">'</span>Accept:application/vnd.lichess.v1+json<span class="pl-pds">'</span></span></pre></div>
<p>Response: <code>200 OK</code></p>
<div class="highlight highlight-source-js"><pre>{
  <span class="pl-c"><span class="pl-c">//</span> see document format in the Play section</span>
}</pre></div>
<p>Use this info to display the challenge.</p>
<p>If you stop receiving the challenge message for some time (lichess web uses 3 seconds),
you should stop displaying it.</p>
</article>
  </div>

  </div>

  <button type="button" data-facebox="#jump-to-line" data-facebox-class="linejump" data-hotkey="l" class="d-none">Jump to Line</button>
  <div id="jump-to-line" style="display:none">
    <!-- '"` --><!-- </textarea></xmp> --></option></form><form accept-charset="UTF-8" action="" class="js-jump-to-line-form" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
      <input class="form-control linejump-input js-jump-to-line-field" type="text" placeholder="Jump to line&hellip;" aria-label="Jump to line" autofocus>
      <button type="submit" class="btn">Go</button>
</form>  </div>

  </div>
  <div class="modal-backdrop js-touch-events"></div>
</div>

    </div>
  </div>

  </div>

      
<div class="footer container-lg px-3" role="contentinfo">
  <div class="position-relative d-flex flex-justify-between py-6 mt-6 f6 text-gray border-top border-gray-light ">
    <ul class="list-style-none d-flex flex-wrap ">
      <li class="mr-3">&copy; 2017 <span title="0.08964s from unicorn-405808628-29454">GitHub</span>, Inc.</li>
        <li class="mr-3"><a href="https://github.com/site/terms" data-ga-click="Footer, go to terms, text:terms">Terms</a></li>
        <li class="mr-3"><a href="https://github.com/site/privacy" data-ga-click="Footer, go to privacy, text:privacy">Privacy</a></li>
        <li class="mr-3"><a href="https://github.com/security" data-ga-click="Footer, go to security, text:security">Security</a></li>
        <li class="mr-3"><a href="https://status.github.com/" data-ga-click="Footer, go to status, text:status">Status</a></li>
        <li><a href="https://help.github.com" data-ga-click="Footer, go to help, text:help">Help</a></li>
    </ul>

    <a href="https://github.com" aria-label="Homepage" class="footer-octicon" title="GitHub">
      <svg aria-hidden="true" class="octicon octicon-mark-github" height="24" version="1.1" viewBox="0 0 16 16" width="24"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8z"/></svg>
</a>
    <ul class="list-style-none d-flex flex-wrap ">
        <li class="mr-3"><a href="https://github.com/contact" data-ga-click="Footer, go to contact, text:contact">Contact GitHub</a></li>
      <li class="mr-3"><a href="https://developer.github.com" data-ga-click="Footer, go to api, text:api">API</a></li>
      <li class="mr-3"><a href="https://training.github.com" data-ga-click="Footer, go to training, text:training">Training</a></li>
      <li class="mr-3"><a href="https://shop.github.com" data-ga-click="Footer, go to shop, text:shop">Shop</a></li>
        <li class="mr-3"><a href="https://github.com/blog" data-ga-click="Footer, go to blog, text:blog">Blog</a></li>
        <li><a href="https://github.com/about" data-ga-click="Footer, go to about, text:about">About</a></li>

    </ul>
  </div>
</div>



  <div id="ajax-error-message" class="ajax-error-message flash flash-error">
    <svg aria-hidden="true" class="octicon octicon-alert" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M8.865 1.52c-.18-.31-.51-.5-.87-.5s-.69.19-.87.5L.275 13.5c-.18.31-.18.69 0 1 .19.31.52.5.87.5h13.7c.36 0 .69-.19.86-.5.17-.31.18-.69.01-1L8.865 1.52zM8.995 13h-2v-2h2v2zm0-3h-2V6h2v4z"/></svg>
    <button type="button" class="flash-close js-flash-close js-ajax-error-dismiss" aria-label="Dismiss error">
      <svg aria-hidden="true" class="octicon octicon-x" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M7.48 8l3.75 3.75-1.48 1.48L6 9.48l-3.75 3.75-1.48-1.48L4.52 8 .77 4.25l1.48-1.48L6 6.52l3.75-3.75 1.48 1.48z"/></svg>
    </button>
    You can't perform that action at this time.
  </div>


    
    <script crossorigin="anonymous" integrity="sha256-dnzP47v4ExBZ2b/KDxJV9pmtXKcB8Chys1Je65HqGIE=" src="https://assets-cdn.github.com/assets/frameworks-767ccfe3bbf8131059d9bfca0f1255f699ad5ca701f02872b3525eeb91ea1881.js"></script>
    
    <script async="async" crossorigin="anonymous" integrity="sha256-AlZypSuYCzlJ6JUsyx0bra11bkKtQvymoaAAI2WkPEU=" src="https://assets-cdn.github.com/assets/github-025672a52b980b3949e8952ccb1d1badad756e42ad42fca6a1a0002365a43c45.js"></script>
    
    
    
    
  <div class="js-stale-session-flash stale-session-flash flash flash-warn flash-banner d-none">
    <svg aria-hidden="true" class="octicon octicon-alert" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M8.865 1.52c-.18-.31-.51-.5-.87-.5s-.69.19-.87.5L.275 13.5c-.18.31-.18.69 0 1 .19.31.52.5.87.5h13.7c.36 0 .69-.19.86-.5.17-.31.18-.69.01-1L8.865 1.52zM8.995 13h-2v-2h2v2zm0-3h-2V6h2v4z"/></svg>
    <span class="signed-in-tab-flash">You signed in with another tab or window. <a href="">Reload</a> to refresh your session.</span>
    <span class="signed-out-tab-flash">You signed out in another tab or window. <a href="">Reload</a> to refresh your session.</span>
  </div>
  <div class="facebox" id="facebox" style="display:none;">
  <div class="facebox-popup">
    <div class="facebox-content" role="dialog" aria-labelledby="facebox-header" aria-describedby="facebox-description">
    </div>
    <button type="button" class="facebox-close js-facebox-close" aria-label="Close modal">
      <svg aria-hidden="true" class="octicon octicon-x" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M7.48 8l3.75 3.75-1.48 1.48L6 9.48l-3.75 3.75-1.48-1.48L4.52 8 .77 4.25l1.48-1.48L6 6.52l3.75-3.75 1.48 1.48z"/></svg>
    </button>
  </div>
</div>


  </body>
</html>

