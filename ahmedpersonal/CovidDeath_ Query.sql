
-- Exploring Data that has NULL in continent
SELECT
	continent,
	location
FROM
	PortfolioProject..CovidDeaths$
WHERE 
	continent IS NULL

GROUP BY 
	continent,
	location


-- Select Data that we are going to use

SELECT 
	location,
	date,
	total_cases,
	new_cases,
	total_deaths,
	population
FROM
	PortfolioProject..CovidDeaths$
ORDER BY
	1,2


-- Looking at Total cases vs Total Deaths per country  / Death Percentage per Country
SELECT
	location,
	population,
	total_death_per_location,
	total_cases_per_location,
	CAST ((total_death_per_location / total_cases_per_location) * 100 AS DECIMAL(4,2)) AS death_percentage
	
FROM(
SELECT
	population,
	location,
	SUM(CAST(total_deaths AS FLOAT)) AS total_death_per_location,
	SUM(CAST(total_cases AS FLOAT)) AS total_cases_per_location
FROM
	PortfolioProject..CovidDeaths$
WHERE
	total_cases IS NOT NULL AND continent IS NOT NULL 
GROUP BY
	location,
	population) AS subquery
ORDER BY
	death_percentage DESC


-- Shows the likelihood of dying if you attract covid in your country
SELECT
	location,
	date,
	total_cases,
	total_deaths,
	(total_deaths/total_cases) * 100 AS death_percentage
FROM
	PortfolioProject..CovidDeaths$
WHERE
	location = 'Tunisia' AND continent IS NOT NULL 

ORDER BY
	death_percentage DESC

-- Looking at the total cases vs the population  in Tunisia
-- Looking at dates where the covid percentage was at least 1%
SELECT
	location,
	date,
	total_cases,
	population,
	infection_rate
FROM
	(SELECT
	location,
	date,
	total_cases,
	population,
	(total_cases/population) * 100 AS infection_rate
	FROM
		PortfolioProject..CovidDeaths$
	WHERE
		location = 'Tunisia' AND continent IS NOT NULL  ) AS subquery
	
WHERE 
	infection_rate >= 1

ORDER BY
	infection_rate DESC

-- looking at countries with highest infection rate compared to population
SELECT 
	location,
	MAX(total_cases)AS highestInfectionCount,
	MAX((total_cases/population) * 100) AS highestInfectionPercentage
FROM
	PortfolioProject..CovidDeaths$
WHERE 
	continent IS NOT NULL 
GROUP BY
	location
ORDER BY 
	3 DESC


-- Showing Countries with highest death count per population

SELECT
	location,
	MAX(CAST(total_deaths AS BIGINT)) AS highestDeathCount
FROM 
	PortfolioProject..CovidDeaths$
WHERE 
	continent IS NOT NULL 
GROUP BY 
	location
ORDER BY
	highestDeathCount DESC


-- LET'S BREAK THINGS DOWN BY CONTINENT
SELECT
	location,
	MAX(CAST(total_deaths AS BIGINT)) AS highestDeathCount
FROM 
	PortfolioProject..CovidDeaths$
WHERE 
	continent IS NULL 
GROUP BY 
	location
ORDER BY
	highestDeathCount DESC

-- Summing the deaths from all countries and grouping them by country
SELECT 
	continent,
	SUM(CAST (total_deaths AS BIGINT)) AS totalDeathsPerContinent
FROM 
	PortfolioProject..CovidDeaths$
WHERE
	continent IS NOT NULL
GROUP BY
	continent



-- numbers per date  
SELECT
	date,
	casesPerDay,
	deathsPerDay,
	(deathsPerDay / casesPerDay) * 100 AS death_percentage_per_day
FROM
(SELECT
	--location,
	date,
	SUM(new_cases) AS casesPerDay,
	SUM(CAST(new_deaths AS INT)) AS deathsPerDay
	--total_deaths,
	--(total_deaths/total_cases) * 100 AS death_percentage
FROM
	PortfolioProject..CovidDeaths$
WHERE
	/*location = 'Tunisia' AND*/ continent IS NOT NULL 
GROUP BY 
	date ) AS SUBQ
ORDER BY
	1,2

-- SUM of deaths and cases 
SELECT
	casesPerDay,
	deathsPerDay,
	(deathsPerDay / casesPerDay) * 100 AS death_percentage
FROM
(SELECT
	SUM(new_cases) AS casesPerDay,
	SUM(CAST(new_deaths AS INT)) AS deathsPerDay
	--total_deaths,
	--(total_deaths/total_cases) * 100 AS death_percentage
FROM
	PortfolioProject..CovidDeaths$
WHERE
	/*location = 'Tunisia' AND*/ continent IS NOT NULL ) AS SUBQ


--looking at total population vs vaccination
-- USING CTE

WITH 
	popvsVac (continent,location,date,population, new_vaccinations, rollingPeopleVaccinated) AS(
SELECT
	dea.continent,
	dea.location,
	dea.date,
	dea.population,
	vac.new_vaccinations,
	-- sums the number of people vaccinated and partitions them by location 
	SUM(CONVERT(BIGINT,vac.new_vaccinations)) OVER (PARTITION BY dea.location ORDER BY  dea.location, dea.date) AS rollingPeopleVaccinated
FROM
	PortfolioProject..CovidVaccinations$ AS vac
INNER JOIN
	PortfolioProject..CovidDeaths$ AS dea 
	ON 
		dea.location = vac.location
		AND dea.date = vac.date
WHERE dea.continent IS NOT NULL
--ORDER BY 2,3
)
SELECT 
	*,
	(rollingPeopleVaccinated/population) * 100
FROM 
	popvsVac
ORDER BY 
	2,3

--USING TEMP TABLE
DROP TABLE if EXISTS percent_population_vaccinated
CREATE TABLE percent_population_vaccinated (
	continent nvarchar(255),
	location nvarchar(255),
	date datetime,
	population numeric,
	new_vaccinations numeric,
	rollingPeopleVaccinated numeric
	)

INSERT INTO percent_population_vaccinated
SELECT
	dea.continent,
	dea.location,
	dea.date,
	dea.population,
	vac.new_vaccinations,
	-- sums the number of people vaccinated and partitions them by location 
	SUM(CONVERT(BIGINT,vac.new_vaccinations)) OVER (PARTITION BY dea.location ORDER BY  dea.location, dea.date) AS rollingPeopleVaccinated
FROM
	PortfolioProject..CovidVaccinations$ AS vac
INNER JOIN
	PortfolioProject..CovidDeaths$ AS dea 
	ON 
		dea.location = vac.location
		AND dea.date = vac.date
WHERE dea.continent IS NOT NULL
--ORDER BY 2,3

SELECT 
	*,
	(rollingPeopleVaccinated/population) * 100
FROM 
	percent_population_vaccinated
ORDER BY 
	2,3


-- CREATING A VIEW
CREATE VIEW percentPopulationVaccinated AS 
SELECT
	dea.continent,
	dea.location,
	dea.date,
	dea.population,
	vac.new_vaccinations,
	-- sums the number of people vaccinated and partitions them by location 
	SUM(CONVERT(BIGINT,vac.new_vaccinations)) OVER (PARTITION BY dea.location ORDER BY  dea.location, dea.date) AS rollingPeopleVaccinated
FROM
	PortfolioProject..CovidVaccinations$ AS vac
INNER JOIN
	PortfolioProject..CovidDeaths$ AS dea 
	ON 
		dea.location = vac.location
		AND dea.date = vac.date
WHERE dea.continent IS NOT NULL;