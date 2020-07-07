package com.g2forge.reassert.maven.modifier;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.reassert.maven.MavenCoordinates;
import com.g2forge.reassert.maven.modifier.IMavenPOMModifier;
import com.g2forge.reassert.maven.modifier.IMavenPOMModifierFactory;
import com.g2forge.reassert.maven.modifier.XSLTMavenPOMModifier;

@Service(IMavenPOMModifierFactory.class)
public class DistributionManagementStatusPOMModifierFactory implements IMavenPOMModifierFactory {
	@Override
	public IMavenPOMModifier getModifier(MavenCoordinates coordinates) {
		final Resource resource = new Resource(getClass(), "distributionmanagement_status.xsl");
		return new XSLTMavenPOMModifier(new ResourceDataSource(resource));
	}
}
