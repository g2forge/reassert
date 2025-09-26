package com.g2forge.reassert.maven.model;

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import com.g2forge.gearbox.maven.packaging.MavenPackaging;
import com.g2forge.reassert.core.model.file.IDescriptor;
import com.g2forge.reassert.maven.MavenCoordinates;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor(onConstructor_ = { @JsonCreator(mode = JsonCreator.Mode.DISABLED) })
@JsonIgnoreProperties(ignoreUnknown = true)
@JacksonXmlRootElement(namespace = "http://maven.apache.org/POM/4.0.0")
public class MavenPOM implements IDescriptor {
	@JsonIgnore
	protected final MavenCoordinates coordinates;

	protected final MavenParent parent;

	@Singular
	protected final List<MavenLicense> licenses;

	@Singular
	protected final List<MavenDependency> dependencies;

	@Singular
	protected final Map<String, String> properties;

	@Singular
	protected final List<String> modules;

	@Singular
	protected final List<MavenProfile> profiles;

	@JsonCreator
	public MavenPOM(String groupId, String artifactId, String version, MavenPackaging packaging, MavenParent parent, List<MavenLicense> licenses, List<MavenDependency> dependencies, Map<String, String> properties, List<String> modules, List<MavenProfile> profiles) {
		this(new MavenCoordinates(null, groupId, artifactId, version, packaging), parent, licenses, dependencies, properties, modules, profiles);
	}

	@JsonIgnore
	@Override
	public boolean isMaterial() {
		return false;
	}
}
