package com.g2forge.reassert.maven.modifier;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.reassert.maven.MavenCoordinates;
import com.g2forge.reassert.maven.modifier.IMavenPOMModifier;
import com.g2forge.reassert.maven.modifier.IMavenPOMModifierFactory;
import com.g2forge.reassert.maven.modifier.XSLTMavenPOMModifier;

@Service(IMavenPOMModifierFactory.class)
public class PluginSurefirePOMModifierFactory implements IMavenPOMModifierFactory {
	protected static final Set<MavenCoordinates> modify;

	static {
		final Set<MavenCoordinates> set = new HashSet<>();
		set.add(new MavenCoordinates(null, "stax", "stax", "1.2.0", null));
		set.add(new MavenCoordinates(null, "xom", "xom", "1.1", null));
		set.add(new MavenCoordinates(null, "commons-httpclient", "commons-httpclient", "3.0", null));
		set.add(new MavenCoordinates(null, "commons-net", "commons-net", "1.4.1", null));
		set.add(new MavenCoordinates(null, "struts", "struts", "1.2.8", null));
		modify = Collections.unmodifiableSet(set);
	}

	@Override
	public IMavenPOMModifier getModifier(MavenCoordinates coordinates) {
		final MavenCoordinates raw = coordinates.toBuilder().system(null).packaging(null).build();
		if (!modify.contains(raw)) return null;

		final Resource resource = new Resource(getClass(), "plugin_surefire.xsl");
		return new XSLTMavenPOMModifier(new ResourceDataSource(resource));
	}
}
