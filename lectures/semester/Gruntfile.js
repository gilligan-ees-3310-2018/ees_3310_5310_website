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
				' * reveal.js <%= pkg.version %> (<%= grunt.template.today("yyyy-mm-dd, HH:MM") %>)\n' +
				' * http://lab.hakim.se/reveal-js\n' +
				' * MIT licensed\n' +
				' *\n' +
				' * Copyright (C) 2016 Hakim El Hattab, http://hakim.se\n' +
				' */'
		},

		qunit: {
			files: [ 'library/reveal.js-3.3.0/test/*.html' ]
		},

		uglify: {
			options: {
				banner: '<%= meta.banner %>\n'
			},
			build: {
				src: 'library/reveal.js-3.3.0/js/reveal.js',
				dest: 'library/reveal.js-3.3.0/js/reveal.min.js'
			}
		},

		sass: {
			core: {
				files: {
				    'library/reveal.js-3.3.0/css/reveal.css': 'library/reveal.js-3.3.0/css/reveal.scss'
					}
			},
			themes: {
				files: [
					{
						expand: true,
						cwd: 'library/reveal.js-3.3.0/css/theme/source',
						src: ['*.scss'],
						dest: 'library/reveal.js-3.3.0/css/theme',
						ext: '.css'
					},
	        {
		        expand: true,
		        cwd: 'assets/css/theme/source',
		        src: ['*.scss'],
		        dest: 'assets/css/theme',
		        ext: '.css'
	        },
	        {
		        expand: true,
		        cwd: 'library/assets/css/theme/source',
		        src: ['*.scss'],
		        dest: 'lecture_lib/library/assets/css/theme',
		        ext: '.css'
					}
				]
			}
		},

		autoprefixer: {
			dist: {
				src: 'library/reveal.js-3.3.0/css/reveal.css'
			}
		},

		cssmin: {
			compress: {
				files: {
					'library/reveal.js-3.3.0/css/reveal.min.css': [ 'library/reveal.js-3.3.0/css/reveal.css' ]
				}
			}
		},

		jshint: {
			options: {
				curly: false,
				eqeqeq: true,
				immed: true,
				latedef: "nofunc",
				newcap: true,
				noarg: true,
				sub: true,
				undef: true,
				eqnull: true,
				browser: true,
				expr: true,
				globals: {
					head: false,
					module: false,
					console: false,
					unescape: false,
					define: false,
					exports: false
				}
			},
			files: [ 'Gruntfile.js', 'library/reveal.js-3.3.0/js/reveal.js' ]
		},

		connect: {
			server: {
				options: {
				  hostname: 'localhost',
					port: port,
					base: base,
					livereload: true,
					open: true
				}
			}
		},

		sync: {
		  main: {
		    files: [{
		      cwd: 'lecture_lib/assets/css/theme/',
		      src: ['*.css'],
	        dest: 'lecture_lib/library/assets/css/theme/',
		    }]
			}
		},

		watch: {
            sync: {
              files: [ 'assets/css/theme/**' ],
              tasks: 'sync'
            },
			js: {
				files: [ 'Gruntfile.js', 'library/reveal.js-3.3.0/js/reveal.js' ],
				tasks: 'js'
			},
			theme: {
			    files: [ 'library/reveal.js-3.3.0/css/theme/source/*.scss', 'library/reveal.js-3.3.0/css/theme/template/*.scss',
				     'assets/css/theme/*.css', 'assets/css/theme/source/*.scss' ],
				tasks: 'css-themes'
			},
			css: {
				files: [ 'library/reveal.js-3.3.0/css/reveal.scss' ],
				tasks: 'css-core'
			},
			html: {
				files: [ '*.html']
			},
			markdown: {
				files: [ '*.md' ]
			},
			options: {
				livereload: true
			}
		}

	});

	// Dependencies
	grunt.loadNpmTasks( 'grunt-contrib-qunit' );
	grunt.loadNpmTasks( 'grunt-contrib-jshint' );
	grunt.loadNpmTasks( 'grunt-contrib-cssmin' );
	grunt.loadNpmTasks( 'grunt-contrib-uglify' );
	grunt.loadNpmTasks( 'grunt-contrib-watch' );
	grunt.loadNpmTasks( 'grunt-sass' );
	grunt.loadNpmTasks( 'grunt-contrib-connect' );
	grunt.loadNpmTasks( 'grunt-autoprefixer' );

	// Default task
	grunt.registerTask( 'default', [ 'css', 'js' ] );

	// JS task
	grunt.registerTask( 'js', [ 'jshint', 'uglify'] );

	// Theme CSS
	grunt.registerTask( 'css-themes', [ 'sass:themes' ] );

	// Core framework CSS
	grunt.registerTask( 'css-core', [ 'sass:core', 'autoprefixer', 'cssmin' ] );

	// All CSS
	grunt.registerTask( 'css', [ 'sass', 'autoprefixer', 'cssmin' ] );

	// Serve presentation locally
	grunt.registerTask( 'serve', [ 'connect', 'watch' ] );

	// Run tests
	grunt.registerTask( 'test', [ 'jshint', 'qunit' ] );

};
