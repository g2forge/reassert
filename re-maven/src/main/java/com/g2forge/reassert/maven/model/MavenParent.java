package com.g2forge.reassert.maven.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.g2forge.gearbox.maven.packaging.MavenPackaging;
import com.g2forge.reassert.maven.MavenCoordinates;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor(onConstructor_ = { @JsonCreator(mode = JsonCreator.Mode.DISABLED) })
@JsonIgnoreProperties(ignoreUnknown = true)
public class MavenParent {
	@JsonIgnore
	protected final MavenCoordinates coordinates;

	protected final String relativePath;

	@JsonCreator
	public MavenParent(String groupId, String artifactId, String version, MavenPackaging packaging, String relativePath) {
		this(new MavenCoordinates(null, groupId, artifactId, version, packaging), relativePath);
	}
}
