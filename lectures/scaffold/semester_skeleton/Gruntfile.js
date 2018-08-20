/* global module:false */
module.exports = function(grunt) {
	var port = grunt.option('port') || 8000;
	var base = grunt.option('base') || '.';

	// Project configuration
	grunt.initConfig({
		pkg: grunt.file.readJSON('package.json'),
		meta: {
			banner:
				'/*!\n' +
				' * Lecture Server <%= pkg.version %> (<%= grunt.template.today("yyyy-mm-dd, HH:MM") %>)\n' +
				' * http://my.vanderbilt.edu/jonathangilligan\n' +
				' * MIT licensed\n' +
				' *\n' +
				' * Copyright (C) 2015 Jonathan Gilligan\n' +
				' */'
		},

		connect: {
			server: {
				options: {
					port: port,
					base: base,
					livereload: true,
					open: false
				}
			}
		},


		watch: {
			options: {
				livereload: true
			},
			js: {
				files: [ '**/*.js']
			},
			css: {
				files: [ '**/*.css' ],
			},
			html: {
				files: [ '**/*.htm*']
			},
            images: {
                files: [ '**/*.png', '**/*.jpg' ]
            }
		}

	});

	// Dependencies
	grunt.loadNpmTasks( 'grunt-contrib-watch' );
	grunt.loadNpmTasks( 'grunt-contrib-connect' );

	// Serve presentation locally
	grunt.registerTask( 'serve', [ 'connect', 'watch' ] );

	// Default task
	grunt.registerTask( 'default', [ 'serve' ] );
};
