package com.g2forge.reassert.maven.modifier;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.reassert.maven.MavenCoordinates;

@Service(IMavenPOMModifierFactory.class)
public class MavenPOMModifierFactory implements IMavenPOMModifierFactory {
	@Override
	public IMavenPOMModifier getModifier(MavenCoordinates coordinates) {
		final Resource resource = new Resource(getClass(), computeTransformResource(coordinates));
		if (!resource.isExists()) return null;

		return new XSLTMavenPOMModifier(new ResourceDataSource(resource));
	}

	protected String computeTransformResource(MavenCoordinates coordinates) {
		final StringBuilder retVal = new StringBuilder();
		retVal.append(coordinates.getGroupId().replace('.', '/').replace('-', '_')).append('/').append(coordinates.getArtifactId().replace('-', '_')).append('/').append("v" + coordinates.getVersion().replaceAll("[-.]", "_"));
		retVal.append("/transform.xsl");
		return retVal.toString();
	}
}
