class EPMVisualization {
    constructor() {
        this.gridSize = 54; 
        this.cellSize = 10;
        this.timeStep = 0;
        this.maxTimeSteps = 318; 
        this.isPlaying = false;
        this.speed = 20; 
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
                
                if ((i >= center-1 && i <= center && j >= center-25 && j <= center+25) || 
                    (j >= center-1 && j <= center && i >= center-25 && i <= center+25)) {
                    
                    let color = 'white';
                    
                    if (i >= center-1 && i <= center && j >= center-1 && j <= center) {
                        color = 'black';
                    }
                    else if ((i < center-1 && j >= center-1 && j <= center) || 
                        (i > center && j >= center-1 && j <= center)) {   
                        color = '#387A96'; 
                    } else if ((j > center && i >= center-1 && i <= center) || 
                              (j < center-1 && i >= center-1 && i <= center)) { 
                        color = '#BE514E'; 
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

        this.mouse = svg.append('circle')
            .attr('r', this.cellSize/3)
            .attr('fill', '#FFE599');

        const center = Math.floor(this.gridSize/2);
        this.mouse
            .attr('cx', (center - 1) * this.cellSize + this.cellSize/2)
            .attr('cy', (center - 1) * this.cellSize + this.cellSize/2);
    }

    getCellType(i, j) {
        const center = Math.floor(this.gridSize/2);
        
        if (Math.abs(i - center) <= 1 && Math.abs(j - center) <= 1) {
            return 'center-zone';
        }
        
        if (j === center && (i < center-1 || i > center+1)) {
            return 'open-arm';
        }
        
        if (i === center && (j < center-1 || j > center+1)) { 
            return 'closed-arm';
        }

        return null;
    }

    generateExploratoryPattern() {
        const pattern = [];
        const center = Math.floor(this.gridSize/2);
        
        pattern.push({x: center - 1, y: center - 1}); 
        
        pattern.push({x: center - 2, y: center - 1});
        for (let i = center - 2; i >= center - 25; i--) {
            pattern.push({x: i, y: center - 1});
        }
        pattern.push({x: center - 25, y: center});
        for (let i = center - 25; i <= center - 1; i++) {
            pattern.push({x: i, y: center});
        }
        
        pattern.push({x: center - 1, y: center + 1});
        for (let j = center + 1; j <= center + 25; j++) {
            pattern.push({x: center - 1, y: j});
        }
        pattern.push({x: center, y: center + 25});
        for (let j = center + 25; j >= center - 1; j--) {
            pattern.push({x: center, y: j});
        }
        
        pattern.push({x: center + 1, y: center - 1});
        for (let i = center + 1; i <= center + 25; i++) {
            pattern.push({x: i, y: center - 1});
        }
        pattern.push({x: center + 25, y: center});
        for (let i = center + 25; i >= center; i--) {
            pattern.push({x: i, y: center});
        }
        
        pattern.push({x: center - 1, y: center - 2});
        for (let j = center - 2; j >= center - 25; j--) {
            pattern.push({x: center - 1, y: j});
        }
        pattern.push({x: center, y: center - 25});
        for (let j = center - 25; j <= center - 1; j++) {
            pattern.push({x: center, y: j});
        }

        const finalPosition = {x: center - 1, y: center - 1}; 
        while (pattern.length < 318) {
            pattern.push(finalPosition);
        }

        return pattern;
    }

    generateDelayedPattern() {
        const pattern = [];
        const center = Math.floor(this.gridSize/2);
        
        pattern.push({x: center - 1, y: center - 1}); 
        
        pattern.push({x: center - 1, y: center + 1});
        for (let j = center + 1; j <= center + 25; j++) {
            pattern.push({x: center - 1, y: j});
        }
        pattern.push({x: center, y: center + 25});
        for (let j = center + 25; j >= center - 1; j--) {
            pattern.push({x: center, y: j});
        }
        
        pattern.push({x: center - 1, y: center - 2});
        for (let j = center - 2; j >= center - 25; j--) {
            pattern.push({x: center - 1, y: j});
        }
        pattern.push({x: center, y: center - 25});
        for (let j = center - 25; j <= center - 1; j++) {
            pattern.push({x: center, y: j});
        }

        for (let i = 0; i < 106; i++) {
            pattern.push({x: center - 1, y: center - 1});
        }
        
        pattern.push({x: center - 2, y: center - 1});
        for (let i = center - 2; i >= center - 25; i--) {
            pattern.push({x: i, y: center - 1});
        }
        pattern.push({x: center - 25, y: center});
        for (let i = center - 25; i <= center - 1; i++) {
            pattern.push({x: i, y: center});
        }
        
        pattern.push({x: center + 1, y: center - 1});
        for (let i = center + 1; i <= center + 25; i++) {
            pattern.push({x: i, y: center - 1});
        }
        pattern.push({x: center + 25, y: center});
        for (let i = center + 25; i >= center; i--) {
            pattern.push({x: i, y: center});
        }

        return pattern;
    }

    generateAvoidantPattern() {
        const pattern = [];
        const center = Math.floor(this.gridSize/2);
        
        pattern.push({x: center - 1, y: center - 1}); 
        
        pattern.push({x: center - 1, y: center + 1});
        for (let j = center + 1; j <= center + 25; j++) {
            pattern.push({x: center - 1, y: j});
        }
        pattern.push({x: center, y: center + 25});
        for (let j = center + 25; j >= center - 1; j--) {
            pattern.push({x: center, y: j});
        }
        
        for (let i = 0; i < 5; i++) {
            pattern.push({x: center - 1, y: center - 1});
        }
        
        pattern.push({x: center - 1, y: center - 2});
        for (let j = center - 2; j >= center - 25; j--) {
            pattern.push({x: center - 1, y: j});
        }
        pattern.push({x: center, y: center - 25});
        for (let j = center - 25; j <= center - 1; j++) {
            pattern.push({x: center, y: j});
        }

        const remainingSteps = this.maxTimeSteps - pattern.length;
        for (let i = 0; i < remainingSteps; i++) {
            pattern.push({x: center - 1, y: center - 1});
        }

        return pattern;
    }

    setupGraphs() {
        const createGraph = (selector, lineColor) => {
            const margin = { top: 10, right: 20, bottom: 30, left: 40 };
            const width = 400 - margin.left - margin.right;
            const height = 150 - margin.top - margin.bottom;

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
            d3.select('#play-pause').text(this.isPlaying ? 'Pause' : 'Play');
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
        
        const openPercentage = (openSquares / 100) * 100;    
        const closedPercentage = (closedSquares / 100) * 100; 
        const totalPercentage = (totalSquares / 200) * 100;   
        
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
            .attr('cx', (center - 1) * this.cellSize + this.cellSize/2)
            .attr('cy', (center - 1) * this.cellSize + this.cellSize/2);

        this.graphs.open.select('.line').attr('d', null);
        this.graphs.closed.select('.line').attr('d', null);
        this.graphs.total.select('.line').attr('d', null);

        d3.select('#play-pause').text('Play');
    }
}

document.addEventListener('DOMContentLoaded', () => {
    new EPMVisualization();
});
