module.exports = function (config) {
    config.set({
        reporters: ['progress', 'junit'],
	customLaunchers: {
	    ChromeHeadlessNoSandbox: {
		base: 'ChromeHeadless',
		flags: ['--no-sandbox']
	    }
	},
	browsers: ['ChromeHeadlessNoSandbox'],
        // The directory where the output file lives
        basePath: 'target',
        // The file itself
        files: ['ci.js'],
        frameworks: ['cljs-test'],
        plugins: ['karma-junit-reporter',
		  'karma-cljs-test',
		  'karma-chrome-launcher'
		 ],
        colors: true,
        logLevel: config.LOG_INFO,
        client: {
            args: ["shadow.test.karma.init"],
            singleRun: true
        },
	junitReporter: {
	    useBrowserName: false
	}
    })
};
