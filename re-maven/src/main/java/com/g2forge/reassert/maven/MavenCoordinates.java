package com.g2forge.reassert.maven;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.maven.model.MavenPackaging;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
@JsonIgnoreProperties("material")
public class MavenCoordinates implements ICoordinates {
	@JsonIgnore
	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	protected final MavenSystem system;

	protected final String groupId;

	protected final String artifactId;

	protected final String version;

	@JsonInclude(Include.NON_NULL)
	@Builder.Default
	protected final MavenPackaging packaging = MavenPackaging.JAR;

	@EqualsAndHashCode.Include(replaces = "version")
	protected String getVersionLowercase() {
		final String version = getVersion();
		if (version == null) return null;
		return version.toLowerCase();
	}
}
