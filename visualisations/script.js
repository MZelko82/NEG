class EPMVisualization {
    constructor() {
        this.gridSize = 34; 
        this.cellSize = 18;
        this.timeStep = 0;
        this.maxTimeSteps = 318; 
        this.isPlaying = false;
        this.speed = 10; 
        this.currentPhenotype = 'exploratory';
        
        this.visitedSquares = {
            open: new Set(),
            closed: new Set()
        };

        this.setupMaze();
        this.setupGraphs();
        this.setupControls();
        this.setupMetrics();
        
        this.currentPattern = this.generatePattern(this.currentPhenotype);
    }

    generatePattern(phenotype) {
        switch(phenotype) {
            case 'exploratory':
                return this.generateExploratoryPattern();
            case 'delayed':
                return this.generateDelayedPattern();
            case 'avoidant':
                return this.generateAvoidantPattern();
            default:
                return this.generateExploratoryPattern();
        }
    }

    setupMaze() {
        const svg = d3.select('#maze')
            .append('svg')
            .attr('width', this.gridSize * this.cellSize)
            .attr('height', this.gridSize * this.cellSize);

        for (let i = 0; i < this.gridSize; i++) {
            for (let j = 0; j < this.gridSize; j++) {
                const center = Math.floor(this.gridSize/2);
                
                if ((i === center && j >= center-15 && j <= center+15) || 
                    (j === center && i >= center-15 && i <= center+15)) {
                    
                    let color = 'white';
                    
                    if (i === center && j === center) {
                        color = 'black';
                    }
                    // Closed arms (horizontal)
                    else if (i === center && (j < center || j > center)) {   
                        color = '#BE514E'; 
                    } 
                    // Open arms (vertical)
                    else if (j === center && (i < center || i > center)) { 
                        color = '#387A96'; 
                    }

                    svg.append('rect')
                        .attr('class', 'grid-cell')
                        .attr('x', j * this.cellSize)
                        .attr('y', i * this.cellSize)
                        .attr('width', this.cellSize)
                        .attr('height', this.cellSize)
                        .style('fill', color)
                        .style('stroke', '#ddd');
                }
            }
        }

        const center = Math.floor(this.gridSize/2);
        
        svg.append('text')
            .attr('x', center * this.cellSize)
            .attr('y', (center - 15.5) * this.cellSize)
            .attr('text-anchor', 'middle')
            .attr('fill', '#387A96')
            .style('font-weight', 'bold')
            .text('Open Arms');

        svg.append('text')
            .attr('x', (center - 12) * this.cellSize)
            .attr('y', (center - 0.5) * this.cellSize)
            .attr('text-anchor', 'middle')
            .attr('fill', '#BE514E')
            .style('font-weight', 'bold')
            .text('Closed Arms');

        this.mouse = svg.append('circle')
            .attr('r', this.cellSize/3)
            .attr('fill', '#FFE599');

        this.mouse
            .attr('cx', center * this.cellSize + this.cellSize/2)
            .attr('cy', center * this.cellSize + this.cellSize/2);
    }

    getCellType(i, j) {
        const center = Math.floor(this.gridSize/2);
        
        if (i === center && j === center) {
            return 'center-zone';
        }
        
        if (j === center && i !== center) {
            return 'open-arm';
        }
        
        if (i === center && j !== center) { 
            return 'closed-arm';
        }

        return null;
    }

    generateExploratoryPattern() {
        const pattern = [];
        const center = Math.floor(this.gridSize/2);
        
        pattern.push({x: center, y: center}); 
        
        for (let i = center - 1; i >= center - 15; i--) {
            pattern.push({x: i, y: center});
        }
        for (let i = center - 15; i <= center; i++) {
            pattern.push({x: i, y: center});
        }
        
        for (let j = center + 1; j <= center + 15; j++) {
            pattern.push({x: center, y: j});
        }
        for (let j = center + 15; j >= center; j--) {
            pattern.push({x: center, y: j});
        }
        
        for (let i = center + 1; i <= center + 15; i++) {
            pattern.push({x: i, y: center});
        }
        for (let i = center + 15; i >= center; i--) {
            pattern.push({x: i, y: center});
        }
        
        for (let j = center - 1; j >= center - 15; j--) {
            pattern.push({x: center, y: j});
        }
        for (let j = center - 15; j <= center; j++) {
            pattern.push({x: center, y: j});
        }

        while (pattern.length < this.maxTimeSteps) {
            pattern.push({x: center, y: center});
        }

        return pattern;
    }

    generateDelayedPattern() {
        const pattern = [];
        const center = Math.floor(this.gridSize/2);
        
        pattern.push({x: center, y: center}); 
        
        for (let j = center + 1; j <= center + 15; j++) {
            pattern.push({x: center, y: j});
        }
        for (let j = center + 15; j >= center; j--) {
            pattern.push({x: center, y: j});
        }
        
        for (let j = center - 1; j >= center - 15; j--) {
            pattern.push({x: center, y: j});
        }
        for (let j = center - 15; j <= center; j++) {
            pattern.push({x: center, y: j});
        }
        
        for (let i = 0; i < 106; i++) {
            pattern.push({x: center, y: center});
        }
        
        for (let i = center - 1; i >= center - 15; i--) {
            pattern.push({x: i, y: center});
        }
        for (let i = center - 15; i <= center; i++) {
            pattern.push({x: i, y: center});
        }
        
        for (let i = center + 1; i <= center + 15; i++) {
            pattern.push({x: i, y: center});
        }
        for (let i = center + 15; i >= center; i--) {
            pattern.push({x: i, y: center});
        }

        while (pattern.length < this.maxTimeSteps) {
            pattern.push({x: center, y: center});
        }

        return pattern;
    }

    generateAvoidantPattern() {
        const pattern = [];
        const center = Math.floor(this.gridSize/2);
        
        pattern.push({x: center, y: center}); 
        
        for (let j = center + 1; j <= center + 15; j++) {
            pattern.push({x: center, y: j});
        }
        for (let j = center + 15; j >= center; j--) {
            pattern.push({x: center, y: j});
        }
        
        for (let i = 0; i < 5; i++) {
            pattern.push({x: center, y: center});
        }
        
        for (let j = center - 1; j >= center - 15; j--) {
            pattern.push({x: center, y: j});
        }
        for (let j = center - 15; j <= center; j++) {
            pattern.push({x: center, y: j});
        }

        while (pattern.length < this.maxTimeSteps) {
            pattern.push({x: center, y: center});
        }

        return pattern;
    }

    setupGraphs() {
        const createGraph = (selector, lineColor) => {
            const margin = { top: 9, right: 18, bottom: 27, left: 36 };
            const width = 360 - margin.left - margin.right;
            const height = 135 - margin.top - margin.bottom;

            const svg = d3.select(selector)
                .append('svg')
                .attr('width', width + margin.left + margin.right)
                .attr('height', height + margin.top + margin.bottom)
                .append('g')
                .attr('transform', `translate(${margin.left},${margin.top})`);

            this.xScale = d3.scaleLinear()
                .domain([0, 300])
                .range([0, width]);

            this.yScale = d3.scaleLinear()
                .domain([0, 100])
                .range([height, 0]);

            svg.append('g')
                .attr('class', 'x-axis')
                .attr('transform', `translate(0,${height})`)
                .call(d3.axisBottom(this.xScale)
                    .tickValues([0, 75, 150, 225, 300])
                    .tickFormat(d => d + 's'));

            svg.append('g')
                .attr('class', 'y-axis')
                .call(d3.axisLeft(this.yScale)
                    .tickValues([0, 25, 50, 75, 100])
                    .tickFormat(d => d + '%'));

            svg.append('path')
                .attr('class', 'line')
                .attr('fill', 'none')
                .attr('stroke', lineColor)
                .attr('stroke-width', 7);

            return svg;
        };

        this.graphs = {
            open: createGraph('#open-arms-graph', '#387A96'),    
            closed: createGraph('#closed-arms-graph', '#BE514E'), 
            total: createGraph('#total-graph', '#BE94C8')       
        };

        this.metricsData = {
            open: [],
            closed: [],
            total: []
        };
    }

    setupControls() {
        d3.select('#phenotype-selector').on('change', (event) => {
            this.currentPhenotype = event.target.value;
            this.reset();
        });

        d3.select('#play-pause').on('click', () => {
            this.isPlaying = !this.isPlaying;
            d3.select('#play-pause').text(this.isPlaying ? 'Stop' : 'Play');
            if (this.isPlaying) this.animate();
        });

        d3.select('#reset').on('click', () => this.reset());
    }

    setupMetrics() {
        // No-op
    }

    animate() {
        if (!this.isPlaying || this.timeStep >= this.maxTimeSteps) {
            this.isPlaying = false;
            d3.select('#play-pause').text('Play');
            return;
        }

        const position = this.currentPattern[this.timeStep];
        
        this.mouse
            .attr('cx', position.y * this.cellSize + this.cellSize/2)
            .attr('cy', position.x * this.cellSize + this.cellSize/2);

        const center = Math.floor(this.gridSize/2);
        
        if ((position.x < center-1 && position.y >= center-1 && position.y <= center) || 
            (position.x > center && position.y >= center-1 && position.y <= center)) {   
            this.visitedSquares.open.add(`${position.x},${position.y}`);
        }
        else if ((position.y > center && position.x >= center-1 && position.x <= center) || 
                 (position.y < center-1 && position.x >= center-1 && position.x <= center)) { 
            this.visitedSquares.closed.add(`${position.x},${position.y}`);
        }

        this.updateMetrics();
        this.timeStep++;

        setTimeout(() => this.animate(), 1000 / this.speed);
    }

    updateMetrics() {
        const openSquares = this.visitedSquares.open.size;
        const closedSquares = this.visitedSquares.closed.size;
        const totalSquares = openSquares + closedSquares;
        
        const openPercentage = (openSquares / 30) * 100;    
        const closedPercentage = (closedSquares / 30) * 100; 
        const totalPercentage = (totalSquares / 60) * 100;   
        
        const currentTime = (this.timeStep / 318) * 300;
        
        this.metricsData.open.push({time: currentTime, value: openPercentage});
        this.metricsData.closed.push({time: currentTime, value: closedPercentage});
        this.metricsData.total.push({time: currentTime, value: totalPercentage});

        const line = d3.line()
            .x(d => this.xScale(d.time))
            .y(d => this.yScale(d.value))
            .curve(d3.curveMonotoneX);

        this.graphs.open.select('.line')
            .datum(this.metricsData.open)
            .attr('d', line);

        this.graphs.closed.select('.line')
            .datum(this.metricsData.closed)
            .attr('d', line);

        this.graphs.total.select('.line')
            .datum(this.metricsData.total)
            .attr('d', line);
    }

    reset() {
        this.timeStep = 0;
        this.isPlaying = false;
        this.visitedSquares = {
            open: new Set(),
            closed: new Set()
        };
        this.metricsData = {
            open: [],
            closed: [],
            total: []
        };
        
        this.currentPattern = this.generatePattern(this.currentPhenotype);
        
        const center = Math.floor(this.gridSize/2);
        this.mouse
            .attr('cx', center * this.cellSize + this.cellSize/2)
            .attr('cy', center * this.cellSize + this.cellSize/2);

        this.graphs.open.select('.line').attr('d', null);
        this.graphs.closed.select('.line').attr('d', null);
        this.graphs.total.select('.line').attr('d', null);

        d3.select('#play-pause').text('Play');
    }
}

document.addEventListener('DOMContentLoaded', () => {
    new EPMVisualization();
});
