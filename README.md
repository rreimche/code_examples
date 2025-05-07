# Some code examples authored by me

## An LLM-Based Multi-Agent System for review of scientific literature.

A system that uses given LLM model (provided by, for instance, Grok or HuggingFace) and arXiv eprint ids to write a literature review. Code: https://github.com/rreimche/genai-exam/tree/main

## A web portal to manage orders

A web app built with symphony 6 to digitalize order management for services in machinery industry related management of a certain family of industrial equipment. It implements an order pipeline: placing a complex order by a client, doing measurements and uploading the results, finishing the order etc.

https://github.com/rreimche/wear-tester/tree/main

## SEP

Here you can find complete code of one package of a network card game implementation. The work was done in the summer of 2015 in as part of my Bachelor study project (SEM is for "Softwareentwicklungsprojekt"). The game implemented is "Cosmic Eidex" and allows several players to play on different computers. The implementation uses Client-Server architecture and makes heavy use of concurrency and RPC tools provided by Java. Inside SEP folder you will see 3 directories corresponding to different modules that were supposed to be deployed on client, server or both (common).

The most interesting parts are probably module_common/spiel/Spiel.java, module_server/spiel/Spielpartie.java and especially their corresponding implementations (SpielImpl.java and SpielPartieImpl.java). The test/spiel/KartenComparatorMitFarbeTest.java can also be interesting.

The project was never meant to go into production neither to be developed further beseides the exam, that's why mostly only APIs of the classes were well-documented.

I am only presenting the part of the application code that I have personally written, because I don't have a corresponding allowance from my team members to publish their parts. The project was meant to be done in teams of 4-5 persons. We have done it in a team of 3 and we have received the highest grade for what we have done.

## OSM_CoinPlacement

These examples are taken from a recently taken research project that is obligatory for my future M.Sc. degree. As a team of 5-6 persons we were building a network game that was meant to be played on mobile devices as front-end clients (we've built an Android app for that) and the back-end part was represented by an architecture builtd on AWS Lambad, AWS Amplify and AWS DynamoDB. This is a subset of classes that were used in an AWS Lambda function, that took a square georgaphical region represented by a pair of geocoordinates, loaded the corresponding geographical region from Open Street Maps, placed game artifacts on walkable ways inside it and saved the map data into DynamoDB. The part that I have written is responsible for the placement of game artifacts.

The ProcessorOfWays.java seems for me to be the most interesting class to look at.

You will not find all the files to compile the Lambda Function because of the same reason – I do not have such an allowance from my team members to publish their parts.

## Minidote

Minidote is a very simplified version of Antidote – a planet scale, highly available, transactional database built on Conflict-free Replicated Datatypes (CRDT) technology (https://github.com/AntidoteDB/antidote). It was written using Erlang OTP in a team of 2 as an excercise to get the allowance to take the exam in "Programming Distributed Systems" course during my Masters study. My part was to program the main logic of the system so that a distributed database system consisting of multiple replicas using CRDTs is able to process updates and reads. The communication between the replicas (causal broadcast) as well as the interfacing part and some of the required integrations are absent herer because of the lack of allowance for that from their authors.

The most interesting file is the minidote_server.erl

## Hunger

This code represents an iOS client for another project in the area of distributed systems: a mobile multiplayer game, in which players use their mobile phones and act as either zombies trying to eat humans or humans who try to survive longer. The seerver part was implemented on the Google Firebare. The code can be found here: https://github.com/rreimche/hunger

## kariera-centr.ru

A web app built with ruby on rails -- a custom made CMS for a company to manage their website: https://github.com/rreimche/kariera-centr.ru

## Twitter scraper

This twitter scraper was written for my bachelor thesis named "Comparison of the diffusion of real and fake news in social networks". It helps to gather data on real and fake news in this social network for further analysis. It uses MongoDB to store the data.

https://github.com/rreimche/newsscrapper

## hedgehog

A web app build with django -- a custom made CMS for a yoga teacher to manage their website: https://github.com/rreimche/hedgehog

## Analisys of information diffusion in social networks

Software I wrote for my bachelor thesis: https://github.com/rreimche/infdiffusion

## Several Shopware Plugins

https://github.com/rreimche/ItReimcheGoogleReviews/tree/main
https://github.com/rreimche/ItReimcheHideSoldOutProducts/tree/main
https://github.com/rreimche/ItReimcheSetGutscheinTaxId/tree/main
https://github.com/rreimche/RreimcheImportEET





