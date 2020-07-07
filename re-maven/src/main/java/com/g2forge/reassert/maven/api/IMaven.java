package com.g2forge.reassert.maven.api;

import java.nio.file.Path;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.gearbox.command.converter.IMethodArgument;
import com.g2forge.gearbox.command.converter.dumb.ArgumentRenderer;
import com.g2forge.gearbox.command.converter.dumb.Command;
import com.g2forge.gearbox.command.converter.dumb.IArgumentRenderer;
import com.g2forge.gearbox.command.converter.dumb.Named;
import com.g2forge.gearbox.command.converter.dumb.Working;
import com.g2forge.gearbox.command.process.IProcess;
import com.g2forge.gearbox.command.proxy.method.ICommandInterface;
import com.g2forge.reassert.maven.MavenCoordinates;
import com.g2forge.reassert.maven.MavenCoordinatesDescriber;

public interface IMaven extends ICommandInterface {
	public static class MavenCoordinatesArgumentRenderer implements IArgumentRenderer<MavenCoordinates> {
		@Override
		public List<String> render(IMethodArgument<MavenCoordinates> argument) {
			final String name = MavenCoordinatesDescriber.create().describe(argument.get()).getName();
			final Named named = argument.getMetadata().get(Named.class);
			return HCollection.asList((named == null ? "" : named.value()) + name);
		}
	}

	public static final Pattern PATTERN_MISSINGARTIFACT = Pattern.compile("Failed to execute goal org\\.apache\\.maven\\.plugins:maven-dependency-plugin:([0-9]+(\\.[0-9]+)*):copy \\(default-cli\\) on project standalone-pom: Unable to find artifact\\.:");

	@Command({ "mvn", "dependency:copy" })
	public IProcess dependencyCopy(@Working Path path, @ArgumentRenderer(MavenCoordinatesArgumentRenderer.class) @Named("-Dartifact=") MavenCoordinates artifact, @Named("-DoutputDirectory=") Path outputDirectory);

	@Command({ "mvn", "help:effective-pom", "--non-recursive" })
	public Stream<String> effectivePOM(@Working Path path, @Named("-Doutput=") Path output);
}
