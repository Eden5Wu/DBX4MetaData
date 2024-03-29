USE [master]
GO
/****** Object:  Database [DBDemos]    Script Date: 03/04/2022 22:15:37 ******/
CREATE DATABASE [DBDemos] ON  PRIMARY 
( NAME = N'ADBDemos', FILENAME = N'C:\SQLDB\ADBDemos.mdf' , SIZE = 4096KB , MAXSIZE = UNLIMITED, FILEGROWTH = 1024KB )
 LOG ON 
( NAME = N'ADBDemos_log', FILENAME = N'C:\SQLDB\ADBDemos_log.ldf' , SIZE = 1024KB , MAXSIZE = 2048GB , FILEGROWTH = 10%)
GO
ALTER DATABASE [DBDemos] SET COMPATIBILITY_LEVEL = 100
GO
IF (1 = FULLTEXTSERVICEPROPERTY('IsFullTextInstalled'))
begin
EXEC [DBDemos].[dbo].[sp_fulltext_database] @action = 'enable'
end
GO
ALTER DATABASE [DBDemos] SET ANSI_NULL_DEFAULT OFF
GO
ALTER DATABASE [DBDemos] SET ANSI_NULLS OFF
GO
ALTER DATABASE [DBDemos] SET ANSI_PADDING OFF
GO
ALTER DATABASE [DBDemos] SET ANSI_WARNINGS OFF
GO
ALTER DATABASE [DBDemos] SET ARITHABORT OFF
GO
ALTER DATABASE [DBDemos] SET AUTO_CLOSE OFF
GO
ALTER DATABASE [DBDemos] SET AUTO_CREATE_STATISTICS ON
GO
ALTER DATABASE [DBDemos] SET AUTO_SHRINK OFF
GO
ALTER DATABASE [DBDemos] SET AUTO_UPDATE_STATISTICS ON
GO
ALTER DATABASE [DBDemos] SET CURSOR_CLOSE_ON_COMMIT OFF
GO
ALTER DATABASE [DBDemos] SET CURSOR_DEFAULT  GLOBAL
GO
ALTER DATABASE [DBDemos] SET CONCAT_NULL_YIELDS_NULL OFF
GO
ALTER DATABASE [DBDemos] SET NUMERIC_ROUNDABORT OFF
GO
ALTER DATABASE [DBDemos] SET QUOTED_IDENTIFIER OFF
GO
ALTER DATABASE [DBDemos] SET RECURSIVE_TRIGGERS OFF
GO
ALTER DATABASE [DBDemos] SET  DISABLE_BROKER
GO
ALTER DATABASE [DBDemos] SET AUTO_UPDATE_STATISTICS_ASYNC OFF
GO
ALTER DATABASE [DBDemos] SET DATE_CORRELATION_OPTIMIZATION OFF
GO
ALTER DATABASE [DBDemos] SET TRUSTWORTHY OFF
GO
ALTER DATABASE [DBDemos] SET ALLOW_SNAPSHOT_ISOLATION OFF
GO
ALTER DATABASE [DBDemos] SET PARAMETERIZATION SIMPLE
GO
ALTER DATABASE [DBDemos] SET READ_COMMITTED_SNAPSHOT OFF
GO
ALTER DATABASE [DBDemos] SET HONOR_BROKER_PRIORITY OFF
GO
ALTER DATABASE [DBDemos] SET  READ_WRITE
GO
ALTER DATABASE [DBDemos] SET RECOVERY SIMPLE
GO
ALTER DATABASE [DBDemos] SET  MULTI_USER
GO
ALTER DATABASE [DBDemos] SET PAGE_VERIFY CHECKSUM
GO
ALTER DATABASE [DBDemos] SET DB_CHAINING OFF
GO
USE [DBDemos]
GO
/****** Object:  Table [dbo].[nextord]    Script Date: 03/04/2022 22:15:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[nextord](
	[NewKey] [float] NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[nextitem]    Script Date: 03/04/2022 22:15:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[nextitem](
	[NewKey] [float] NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[nextcust]    Script Date: 03/04/2022 22:15:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[nextcust](
	[NewCust] [float] NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[items]    Script Date: 03/04/2022 22:15:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[items](
	[OrderNo] [float] NOT NULL,
	[ItemNo] [float] NOT NULL,
	[PartNo] [float] NULL,
	[Qty] [int] NULL,
	[Discount] [float] NULL,
 CONSTRAINT [PK_items] PRIMARY KEY CLUSTERED 
(
	[OrderNo] ASC,
	[ItemNo] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[emps]    Script Date: 03/04/2022 22:15:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[emps](
	[EmpNo] [int] IDENTITY(1,1) NOT NULL,
	[LastName] [nvarchar](20) NULL,
	[FirstName] [nvarchar](15) NULL,
	[PhoneExt] [nvarchar](4) NULL,
	[HireDate] [datetime] NULL,
	[Salary] [float] NULL,
PRIMARY KEY CLUSTERED 
(
	[EmpNo] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[employee]    Script Date: 03/04/2022 22:15:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[employee](
	[EmpNo] [int] NOT NULL,
	[LastName] [nvarchar](20) NULL,
	[FirstName] [nvarchar](15) NULL,
	[PhoneExt] [nvarchar](4) NULL,
	[HireDate] [datetime] NULL,
	[Salary] [float] NULL,
 CONSTRAINT [PK_employee] PRIMARY KEY CLUSTERED 
(
	[EmpNo] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
CREATE NONCLUSTERED INDEX [ix_EmployeeByName] ON [dbo].[employee] 
(
	[LastName] ASC,
	[FirstName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[customer]    Script Date: 03/04/2022 22:15:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[customer](
	[CustNo] [int] NOT NULL,
	[Company] [nvarchar](30) NULL,
	[Addr1] [nvarchar](30) NULL,
	[Addr2] [nvarchar](30) NULL,
	[City] [nvarchar](15) NULL,
	[State] [nvarchar](20) NULL,
	[Zip] [nvarchar](10) NULL,
	[Country] [nvarchar](20) NULL,
	[Phone] [nvarchar](15) NULL,
	[FAX] [nvarchar](15) NULL,
	[TaxRate] [float] NULL,
	[Contact] [nvarchar](20) NULL,
	[LastInvoiceDate] [datetime] NULL,
 CONSTRAINT [PK_customer] PRIMARY KEY CLUSTERED 
(
	[CustNo] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
CREATE NONCLUSTERED INDEX [ix_CustomerByCompany] ON [dbo].[customer] 
(
	[Company] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[country]    Script Date: 03/04/2022 22:15:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[country](
	[Name] [nvarchar](24) NOT NULL,
	[Capital] [nvarchar](24) NULL,
	[Continent] [nvarchar](24) NULL,
	[Area] [float] NULL,
	[Population] [float] NULL,
 CONSTRAINT [PK_country] PRIMARY KEY CLUSTERED 
(
	[Name] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[vendors]    Script Date: 03/04/2022 22:15:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[vendors](
	[VendorNo] [float] NOT NULL,
	[VendorName] [nvarchar](30) NULL,
	[Address1] [nvarchar](30) NULL,
	[Address2] [nvarchar](30) NULL,
	[City] [nvarchar](20) NULL,
	[State] [nvarchar](20) NULL,
	[Zip] [nvarchar](10) NULL,
	[Country] [nvarchar](15) NULL,
	[Phone] [nvarchar](15) NULL,
	[FAX] [nvarchar](15) NULL,
	[Preferred] [bit] NOT NULL,
 CONSTRAINT [PK_vendors] PRIMARY KEY CLUSTERED 
(
	[VendorNo] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[parts]    Script Date: 03/04/2022 22:15:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[parts](
	[PartNo] [float] NOT NULL,
	[VendorNo] [float] NULL,
	[Description] [nvarchar](30) NULL,
	[OnHand] [float] NULL,
	[OnOrder] [float] NULL,
	[Cost] [float] NULL,
	[ListPrice] [float] NULL,
 CONSTRAINT [PK_parts] PRIMARY KEY CLUSTERED 
(
	[PartNo] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[orders]    Script Date: 03/04/2022 22:15:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[orders](
	[OrderNo] [float] NOT NULL,
	[CustNo] [int] NOT NULL,
	[SaleDate] [datetime] NULL,
	[ShipDate] [datetime] NULL,
	[EmpNo] [int] NOT NULL,
	[ShipToContact] [nvarchar](20) NULL,
	[ShipToAddr1] [nvarchar](30) NULL,
	[ShipToAddr2] [nvarchar](30) NULL,
	[ShipToCity] [nvarchar](15) NULL,
	[ShipToState] [nvarchar](20) NULL,
	[ShipToZip] [nvarchar](10) NULL,
	[ShipToCountry] [nvarchar](20) NULL,
	[ShipToPhone] [nvarchar](15) NULL,
	[ShipVIA] [nvarchar](7) NULL,
	[PO] [nvarchar](15) NULL,
	[Terms] [nvarchar](6) NULL,
	[PaymentMethod] [nvarchar](7) NULL,
	[ItemsTotal] [float] NULL,
	[TaxRate] [float] NULL,
	[Freight] [float] NULL,
	[AmountPaid] [float] NULL,
 CONSTRAINT [PK_orders] PRIMARY KEY CLUSTERED 
(
	[OrderNo] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  ForeignKey [FK_orders_customer]    Script Date: 03/04/2022 22:15:37 ******/
ALTER TABLE [dbo].[orders]  WITH CHECK ADD  CONSTRAINT [FK_orders_customer] FOREIGN KEY([CustNo])
REFERENCES [dbo].[customer] ([CustNo])
GO
ALTER TABLE [dbo].[orders] CHECK CONSTRAINT [FK_orders_customer]
GO
/****** Object:  ForeignKey [FK_orders_employee]    Script Date: 03/04/2022 22:15:37 ******/
ALTER TABLE [dbo].[orders]  WITH CHECK ADD  CONSTRAINT [FK_orders_employee] FOREIGN KEY([EmpNo])
REFERENCES [dbo].[employee] ([EmpNo])
GO
ALTER TABLE [dbo].[orders] CHECK CONSTRAINT [FK_orders_employee]
GO
