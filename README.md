![image](https://user-images.githubusercontent.com/98691360/190911801-6d1284ce-f5e0-49aa-9722-404d57207c08.png)

# ETL Pipeline Analytics Dashboard in R
This repo contains a full ETL pipeline coded in R. The script extracts data from Airtable (a spreadsheet-database hybrid), cleans and aggregates the data into meaningful 
statistics, then stores the end result in a G-sheet, which feeds a Data Studio dashboard. I cannot provide the link to the dashboard because the data displayed in it
is owned by the client I did this project for, so I will only provide screenshots at the end of the markdown file to give a gist of how the data was visualized.

# The Objective of the Project
The company I created this pipeline for is an E-commerce startup that sells furniture online in the MENA region. They store data about their sales in a cloud-based 
database called Airtable (see the snippet below). Whenever the startup's executives wanted to analyze something, they had to download the data to an Excel sheet 
and do their own calculations. The process was inefficient and there were no standardized KPIs that the founding team could look at every day to monitor the progress 
of the business.

The goal was thus to come up with a **set of success metrics** that are relevant to the client's business and create a **reproducible** process
that puts this raw data into a format that business stakeholders can easily interpret and make decisions from.

### **A Snapshot of the Transactions Database**
![image](https://user-images.githubusercontent.com/98691360/190912049-a79cfde1-3468-4b9d-8d0d-21e787815c1e.png)

# Steps involved in the process
This project consisted of two parts, one was creating a **data pipeline** that gets triggered every day at 5 PM CET, and the other was visualizing the output data in
Data Studio

## The Data Pipeline Part of the Project
Here, I had to do the following tasks:
- Use my business acumen in the area of E-commerce to define **KPIs that inform the founding team about the performance of their business**
- Create an R script that connects to **Airtable's API** to extract, combine, and clean data from disparate source sheets. There was a lot of string manipulation needed
as the columns contained non-latin characters. Also, some imputation was required because because of data entry errors that resulted in missing values
- Aggregate the clean data into **descriptive statistics** and **properly formatted tables** to be consumed by **Data Studio**
- **Upload the data to a G-sheet**. G-sheet was chosen as the medium of storage as the data size was not big and Data Studio did not easily integrate with Airtable. 
In other sophisticated scenarios, Big Query would be a viable option
- Send a **success Email** informing stakeholders that the dashboard has been updated after the above steps are finished
- **Orchestrate the process** via Windows Task Scheduler

## The Data Visualization Part of the Project
The data pipeline produced a few tables that could be fed to a **dashboard to create some informative charts**. Below is a glimpse of some of the charts I created
for the founding team. The dashboard was designed in a way to enable users to **slice and dice the data along many dimensions**.

## Time Series Charts
![image](https://user-images.githubusercontent.com/98691360/190914153-c52bb598-33d0-4fa5-8d01-b825933349dd.png)

## GMV, Revenue, AOV, and Revenue Split Per Vendor (Only Top 4 Vendors Shown)
![image](https://user-images.githubusercontent.com/98691360/190914005-b3c013ba-5542-45c4-9cc1-61bd08bdc6ce.png)
![image](https://user-images.githubusercontent.com/98691360/190914013-38f7d3e5-ad00-456d-a5dd-2ef975c98d79.png)

## Total Orders, GMV, Revenue, and AOV Per Price Point Interval
![image](https://user-images.githubusercontent.com/98691360/190913892-f5b14f06-5acc-4453-8d62-9e988e543c4a.png)
![image](https://user-images.githubusercontent.com/98691360/190913927-8be666b4-7d69-4cf8-a3f5-2536233f8cfb.png)

